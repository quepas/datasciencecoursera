rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  unique_states <- unique(data[,7])
  if (!state %in% unique_states) {
    stop("invalid state")
  }
  unique_outcomes_idx <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if (!outcome %in% names(unique_outcomes_idx)) {
    stop("invalid outcome")
  }
  unique_num <- c("best", "worst")
  if (!is.numeric(num) && !num %in% unique_num) {
    stop("invalid num")
  }
  ## Return hospital name in that state with the given rank 30-day death rate
  data_from_state <- subset(data, data$State==state)
  morality_idx <- unique_outcomes_idx[outcome]
  data_from_state <- data_from_state[data_from_state[,morality_idx]!="Not Available",]
  data_from_state[, morality_idx] <- as.numeric(data_from_state[,morality_idx])
  ordered_data_from_state <- data_from_state[order(data_from_state[,morality_idx], data_from_state[,2]),];
  hospital_names <- ordered_data_from_state[,2]
  num_hospital <- length(hospital_names)
  
  if (num == "best") {
    return(hospital_names[1])
  }
  if (num == "worst") {
    return(hospital_names[num_hospital])
  }
  if (num > num_hospital) {
    return("NA")
  }
  hospital_names[num]
}