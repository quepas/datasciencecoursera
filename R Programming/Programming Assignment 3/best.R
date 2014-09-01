best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death rate
  data_from_state <- subset(data, data$State==state)
  morality_idx <- unique_outcomes_idx[outcome]
  morality_min <- min(data_from_state[,morality_idx])
  hospital_names <- data_from_state[data_from_state[,morality_idx]==morality_min,2]
  sort(hospital_names)[1]
}