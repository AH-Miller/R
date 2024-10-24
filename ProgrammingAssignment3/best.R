best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv",
                              colClasses = "character")
  
  ## Check that state and outcome are valid
  if(!(state %in% data[,7])) {
    stop("invalid state", call. = FALSE)
  }
  if(!(outcome %in% c("pneumonia", "heart attack", "heart failure"))) {
    stop("invalid outcome", call. = FALSE)
  }
  
  ## Return hospital name in that state with lowest 30-day death rate
  state_data <- data[data[,7] == state,]
  state_data <- state_data[order(state_data[, 2]), ]
  if (outcome == 'pneumonia') {
    min_idx <- suppressWarnings(which.min(state_data[, 23]))
  }
  else if (outcome == 'heart attack') {
    min_idx <- suppressWarnings(which.min(state_data[, 11]))
  }
  else {
    min_idx <- suppressWarnings(which.min(state_data[, 17]))
  }
  
  state_data[min_idx, 2]
}