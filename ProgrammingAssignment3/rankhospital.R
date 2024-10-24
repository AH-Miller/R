rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank 30-day death rate
  state_data <- data[data[,7] == state,]
  if (outcome == 'pneumonia') {
    state_data[, 23] <- suppressWarnings(as.numeric(state_data[, 23]))
    state_data <- state_data[order(state_data[, 2]), ]
    if (num == "worst") {
      state_data <- state_data[order(state_data[, 23], decreasing = TRUE), ]
    } else {
      state_data <- state_data[order(state_data[, 23]), ]
    }
  }
  else if (outcome == 'heart attack') {
    state_data[, 11] <- suppressWarnings(as.numeric(state_data[, 11]))
    state_data <- state_data[order(state_data[, 2]), ]
    if (num == "worst") {
      state_data <- state_data[order(state_data[, 11], decreasing = TRUE), ]
    } else {
      state_data <- state_data[order(state_data[, 11]), ]
    }
  }
  else {
    state_data[, 17] <- suppressWarnings(as.numeric(state_data[, 17]))
    state_data <- state_data[order(state_data[, 2]), ]
    if (num == "worst") {
      state_data <- state_data[order(state_data[, 17], decreasing = TRUE), ]
    } else {
      state_data <- state_data[order(state_data[, 17]), ]
    }
  }
  
  if (num == "best" || num == "worst") {
    state_data[1, 2]
  }
  else {
    state_data[num, 2]
  }
}