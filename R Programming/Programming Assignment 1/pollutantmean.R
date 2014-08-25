pollutantmean <- function(directory, pollutant, id = 1:332) {
  file_names <- list.files(directory, pattern="\\.csv$")

  data <- numeric()
  for (name in file_names) {
    path <- paste(directory, name, sep="\\")
    file <- read.csv(path)
    if (file[1,4] %in% id) {
      data <- c(data, file[, pollutant])
    }
  }
  data <- data[!is.na(data)]
  mean(data)
}
