prepare <- function(experiment_folder, om)
{
    file.copy(file.path(om$path, "densities.csv"), experiment_folder, overwrite=TRUE)
    file.copy(file.path(om$path, paste0("scenario_", om$version, ".xsd")), experiment_folder, overwrite=TRUE)
}

create_commands <- function(scenarios, experiment_folder, om)
{
    commands = list()
    output_format = sub("\\.gz$", "", om$output_format)
    compress_output = endsWith(om$output_format, ".gz")
    has_output_format = om$version >= 50 # the --output-format option was added in version 50

    for(row in seq_len(nrow(scenarios)))
    {
        index = scenarios$index[row]
        outputfile = scenarios$outputFile[row]
        exe <- file.path(om$path, if (.Platform$OS.type == "windows") "openMalaria.exe" else "openMalaria")
        args = c(
            shQuote(exe),
            "-s", shQuote(file.path("xml", paste0(index, ".xml")))
        )
        if (has_output_format) {
            args = c(args, "--output-format", output_format)
        } else if (compress_output) {
            # older builds append ".gz" to the --output file themselves
            outputfile = sub("\\.gz$", "", outputfile)
        }
        if (compress_output) {
            args = c(args, "--compress-output")
        }
        args = c(args, "--output", shQuote(outputfile))
        command = paste(args, collapse = " ")
        commands = append(commands, command)
    }
    writeLines(as.character(commands), file.path(experiment_folder, "commands.txt"))
    
    commands
}

run <- function(scenarios, experiment_folder, om, slurm = NULL, overwrite = FALSE, local_cores = NULL)
{
    if (!om$output_format %in% c("bin", "bin.gz", "txt", "txt.gz")) stop("Unknown output_format: ", om$output_format)

    if (om$version < 50 && om$output_format %in% c("bin", "bin.gz")) {
        stop("Binary output requires OpenMalaria version >= 50 (output_format='", om$output_format,
             "' but version=", om$version, "). Use output_format='txt' or 'txt.gz'.")
    }

    xml_files = file.path(experiment_folder, "xml", paste0(scenarios$index, ".xml"))
    if (!all(file.exists(xml_files))) stop("Scenario XML files are missing. Run write_scenarios() first.")

    folders = file.path(experiment_folder, c("out", "log"))
    existing = folders[file.exists(folders)]
    if (length(existing) && !overwrite) {
        stop("Output folders already exist: ", paste(existing, collapse = ", "), ". Use overwrite = TRUE to replace them.")
    }
    if (overwrite && unlink(folders, recursive=TRUE)) stop("Could not remove output folders")
    if (!all(vapply(folders, dir.create, logical(1), recursive=TRUE))) stop("Could not create output folders")

    scenarios$outputFile = file.path("out", paste0(scenarios$index, ".", om$output_format))
    fwrite(scenarios, file.path(experiment_folder, "scenarios.csv"))
    
    message("Creating commands...")
    commands = create_commands(scenarios, experiment_folder, om)
    
    message("Running scenarios...")
    if (is.null(slurm)) run_local(commands, experiment_folder, om, local_cores) else run_slurm(commands, experiment_folder, om, slurm)
}

run_slurm <- function(commands, experiment_folder, om, slurm)
{
    prepare(experiment_folder, om)
    n <- ceiling(length(commands) / slurm$batch_size)
    
    script <- readLines("job.sh")
    replacements <- c(
        N = n,
        account = slurm$account,
        partition = slurm$partition,
        jobname = slurm$job_name,
        qos = slurm$qos,
        time = slurm$time,
        MEM_PER_CPU = slurm$mem_per_cpu,
        CPUS_PER_TASK = slurm$cpus_per_task,
        BATCH_SIZE = slurm$batch_size
    )
    for (name in names(replacements)) {
        script <- gsub(paste0("@", name, "@"), replacements[[name]], script, fixed = TRUE)
    }
    writeLines(script, file.path(experiment_folder, "start_array_job.sh"))
      
    message("Submitted ", n, " jobs")

    oldwd <- setwd(experiment_folder); on.exit(setwd(oldwd), add = TRUE)
    if (system2("sbatch", c("--wait", "start_array_job.sh")) != 0) stop("Slurm job failed")
}

run_local <- function(commands, experiment_folder, om, cores = NULL)
{
  prepare(experiment_folder, om)
  
  n <- length(commands)
  if (is.null(cores)) cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores)) cores <- parallel::detectCores(logical = TRUE)
  if (is.na(cores)) cores <- 1L
  cores <- max(1L, min(as.integer(cores), n))
  message("Running ", n, " scenarios locally on ", cores, " cores")
  
  run_one <- function(i) {
    cmd <- commands[[i]]
    oldwd <- setwd(experiment_folder); on.exit(setwd(oldwd), add = TRUE)
    
    logfile <- file.path("log", paste0("job_", i, ".log"))
    
    if (.Platform$OS.type == "windows") {
      # cmd.exe does not understand the \" escapes produced by shQuote() on the whole
      # command: wrap it in one extra pair of quotes instead, which cmd /c strips again
      cmd_full <- paste0('"', cmd, ' > "', gsub("/", "\\\\", logfile), '" 2>&1"')
      exitcode <- system2("cmd", c("/c", cmd_full), stdout = NULL, stderr = NULL, wait = TRUE)
    } else {
      # Must quote entire command string for redirection to work
      cmd_full <- paste(cmd, ">", shQuote(logfile), "2>&1")
      exitcode <- system2("bash", paste("-c", shQuote(cmd_full)), stdout = NULL, stderr = NULL, wait = TRUE)
    }
    
    if (exitcode != 0) {
      msg = paste0("Error in task ", i, ": ", cmd, "\n  See log: ", logfile)
      msg
    } else NULL
  }

  if (cores == 1L) {
    results <- lapply(seq_along(commands), run_one)
  } else {
    experiment_folder <- normalizePath(experiment_folder, winslash = "/", mustWork = TRUE)
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    results <- parallel::parLapplyLB(cl, seq_along(commands), run_one)
  }

  cat(paste(unlist(results), collapse = "\n"))
}
