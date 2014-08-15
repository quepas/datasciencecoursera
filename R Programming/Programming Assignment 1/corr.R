corr <- function(directory, threshold = 0) {
  file_names <- list.files(directory, pattern="\\.csv$")

  result <- numeric()
  for (name in file_names) {
    path <- paste(directory, name, sep="\\")
    file <- read.csv(path)

    complete <- complete.cases(file)
    if (sum(complete) > threshold) {
      data <- file[complete,]
      result <- c(result, cor(data$sulfate, data$nitrate))
    }
  }
  result
}
