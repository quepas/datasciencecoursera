corr <- function(directory, threshold = 0) {
  file_names <- list.files(directory, pattern="\\.csv$")

  result <- numeric()
  for (name in file_names) {
    path <- paste(directory, name, sep="\\")
    file <- read.csv(path)

    complete <- complete.cases(file)
    if (sum(complete) > threshold) {
      data <- file[complete,]
      sulfate <- data[, 2]
      nitrate <- data[, 3]
      result <- c(result, cor(sulfate, nitrate))
    }
  }
  result
}
