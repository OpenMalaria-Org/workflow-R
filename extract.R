read_uint64 <- function(bytes) {
  sum(as.double(bytes) * 256^(0:7))
}

read_monitoring_binary <- function(path) {
  con <- if (endsWith(path, ".gz")) gzfile(path, "rb") else file(path, "rb")
  on.exit(close(con))

  header <- readBin(con, raw(), 28L)
  if (length(header) != 28L) stop("Truncated binary output: ", path)

  magic <- header[1:8]
  version <- readBin(header[9:12], integer(), 1L, 4L, endian = "little")
  integer_rows <- read_uint64(header[13:20])
  double_rows <- read_uint64(header[21:28])

  if (!identical(magic, charToRaw("OMOUTBIN")) || version != 2L) {
    stop("Unsupported OpenMalaria binary format: ", path)
  }

  rows <- integer_rows + double_rows
  result <- list(
    survey = readBin(con, integer(), rows, 4L, endian = "little"),
    ageGroup = readBin(con, integer(), rows, 4L, endian = "little"),
    measure = readBin(con, integer(), rows, 4L, endian = "little"),
    value = c(
      readBin(con, integer(), integer_rows, 4L, endian = "little"),
      readBin(con, double(), double_rows, 8L, endian = "little")
    )
  )

  data.table::setDT(result)
  if (nrow(result) != rows || length(readBin(con, raw(), 1L))) {
    stop("Invalid binary output size: ", path)
  }
  result
}

read_monitoring_output <- function(path) {
  if (grepl("\\.bin(\\.gz)?$", path)) {
    read_monitoring_binary(path)
  } else {
    data.table::fread(path, sep = "\t", header = FALSE,
                      col.names = c("survey", "ageGroup", "measure", "value"))
  }
}

to_df <- function(scenarios, experiment_folder)
{
    infoCount = 0
    maxInfoCount = 100
    n = nrow(scenarios)
    data = list()

    for (row in seq_len(n))
    {
        index <- scenarios$index[row]
        f <- if ("outputFile" %in% names(scenarios)) {
            file.path(experiment_folder, scenarios[row, "outputFile"][[1]])
        } else {
            file.path(experiment_folder, "txt", paste0(index, ".txt"))
        }
        if(file.exists(f) == FALSE) {
            if(infoCount < maxInfoCount) {
                message("Warning: File ", f, " is missing")
                if(infoCount == maxInfoCount-1) {
                    message("Further missing files will not be shown")
                }
            }
            infoCount <- infoCount + 1
        }
        else {
            d = read_monitoring_output(f)
            d[,'index'] = index
            data[[row]] <- d
        }
    }
    
    data <- rbindlist(data)
    
    if(infoCount > 0) {
        message("Warning: ", infoCount, "/", n, " files are missing")
    }
    
    if (nrow(data) == 0) {
        message("Error: Dataframe is empty because no outputs were found")
        message("       Check the log files and make sure OpenMalaria is able to run")
    }

    data
}

extract <- function(scenarios, experiment_folder, overwrite = FALSE)
{
    message("Extracting results...")
    output_file = file.path(experiment_folder, "output.rds")
    if (file.exists(output_file) && !overwrite) stop("Output file already exists: ", output_file, ". Use overwrite = TRUE to replace it.")
    
    if (!"outputFile" %in% names(scenarios)) {
        scenarios = fread(file.path(experiment_folder, "scenarios.csv"))
    }
    
    start.time <- Sys.time()
    df = to_df(scenarios, experiment_folder)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    message("Extract time: ", time.taken)
    
    if(nrow(df) == 0) {
        message("Error: extraction failed, output dataframe is empty")
        message("       ", output_file, " not saved")
    }
    else {
        start.time <- Sys.time()
        saveRDS(df, output_file)
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        message("Write time: ", time.taken)
    }
    
    invisible(df)
}
