# Self-check: Rscript test_extract.R
suppressMessages(library(data.table))
source("extract.R")

TEMP = c("out", "log", "xml", "commands.txt", "densities.csv", "scenario_50.xsd")

check <- function(remove_temp_files, missing_output, cleaned) {
    folder = tempfile()
    for (name in c("out", "log", "xml")) dir.create(file.path(folder, name), recursive = TRUE)
    writeLines("cmd", file.path(folder, "commands.txt"))
    writeLines("a", file.path(folder, "densities.csv"))
    writeLines("<xs:schema/>", file.path(folder, "scenario_50.xsd"))
    for (i in if (missing_output) 1 else 1:2) {
        writeLines(c(paste(i, 0, 0, 1.5, sep = "\t"), paste(i, 1, 3, 2.5, sep = "\t")),
                   file.path(folder, "out", paste0(i, ".txt")))
    }
    scenarios = data.table(index = 1:2, seed = 1:2, outputFile = file.path("out", paste0(1:2, ".txt")))

    df = extract(scenarios, folder, overwrite = TRUE, remove_temp_files = remove_temp_files)
    stopifnot(nrow(df) > 0, file.exists(file.path(folder, "output.parquet")))
    for (name in TEMP) stopifnot(file.exists(file.path(folder, name)) != cleaned)
    unlink(folder, recursive = TRUE)
}

check(remove_temp_files = TRUE, missing_output = FALSE, cleaned = TRUE)
check(remove_temp_files = FALSE, missing_output = FALSE, cleaned = FALSE)
check(remove_temp_files = TRUE, missing_output = TRUE, cleaned = FALSE)
cat("OK\n")
