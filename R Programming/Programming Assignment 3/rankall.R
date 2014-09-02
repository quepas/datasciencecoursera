rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  unique_states <- unique(data[,7])
  unique_outcomes_idx <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
  if (!outcome %in% names(unique_outcomes_idx)) {
    stop("invalid outcome")
  }
  unique_num <- c("best", "worst")
  if (!is.numeric(num) && !num %in% unique_num) {
    stop("invalid num")
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result = data.frame(1:length(unique_states),1:length(unique_states))
  names(result) <- c("hospital", "state")
  counter = 1
  for (state in unique_states) {
    data_from_state <- subset(data, data$State==state)
    morality_idx <- unique_outcomes_idx[outcome]
    data_from_state <- data_from_state[data_from_state[,morality_idx]!="Not Available",]
    data_from_state[, morality_idx] <- as.numeric(data_from_state[,morality_idx])
    ordered_data_from_state <- data_from_state[order(data_from_state[,morality_idx], data_from_state[,2]),];
    hospital_names <- ordered_data_from_state[,2]
    num_hospital <- length(hospital_names)
    
    if (num == "best") {
      result[counter,1] = hospital_names[1]
      result[counter,2] = state
    }
    if (num == "worst") {
      result[counter,1] = hospital_names[num_hospital]
      result[counter,2] = state
    }
    if (num > num_hospital) {
      result[counter,1] = "NA"
      result[counter,2] = state
    }
    result[counter,1] = hospital_names[num]
    result[counter,2] = state
    counter = counter + 1
  }
  result
}