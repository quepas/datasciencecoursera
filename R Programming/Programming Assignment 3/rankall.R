rankall <- function(outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  unique_states <- sort(unique(data[,7]))
  num_unique_states <- length(unique_states)
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
  result = data.frame(1:num_unique_states,1:num_unique_states)
  names(result) <- c("hospital", "state")
  morality_idx <- unique_outcomes_idx[outcome]
  counter = 1
  for (state in unique_states) {
    data_from_state <- subset(data, data$State==state)
    data_from_state[, morality_idx] <- as.numeric(data_from_state[,morality_idx])
    data_order <- order(data_from_state[,morality_idx], data_from_state$Hospital.Name)
    ordered_data_from_state <- data_from_state[data_order,];
    hospital_names <- ordered_data_from_state$Hospital.Name
    num_hospital <- length(hospital_names)
    
    if (num == "best") {
      result[counter,1] = hospital_names[1]
    }
    else if (num == "worst") {
      result[counter,1] = hospital_names[num_hospital]
    }
    else if (num > num_hospital) {
      result[counter,1] = "NA"
    }
    else {
      result[counter,1] = hospital_names[num]
    }
    result[counter,2] = state
    counter = counter + 1
  }
  result
}