pollutantmean <- function(directory, pollutant, id = 1:332) {
  file_names <- list.files(directory, pattern="\\.csv$")

  idx <-
    if (pollutant == "sulfate") {
      2
    } else {
      3
    }

  data <- numeric()
  for (name in file_names) {
    path <- paste(directory, name, sep="\\")
    file <- read.csv(path)
    if (file[1,4] %in% id) {
      data <- c(data, file[, idx])
    }
  }
  data <- data[!is.na(data)]
  mean(data)
}