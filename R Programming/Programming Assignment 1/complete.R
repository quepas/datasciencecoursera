complete <- function(directory, id = 1:332) {
  file_names <- list.files(directory, pattern="\\.csv$")

  nobs <- numeric(length(id))
  result <- data.frame(id, nobs)
  for (name in file_names) {
    path <- paste(directory, name, sep="\\")
    file <- read.csv(path)
    file_id <- file[1,4]

    if (file_id %in% id) {
      result[result[, 1] == file_id, 2] <- sum(complete.cases(file))
    }
  }
  result
}
