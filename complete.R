complete <- function (directory, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1 117
  ## 2 1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the ## number of complete cases
  
  nobs <- NULL
  
  for (i in id) {
    if (i < 10) {
      file <- paste0(directory, "/00", as.character(i), ".csv")
    }
    else if (i < 100) {
      file <- paste0(directory, "/0", as.character(i), ".csv")
    }
    else {
      file <- paste0(directory, "/", as.character(i), ".csv")
    }
    data <- read.csv(file)
    
    nobs <- c(nobs, sum(!is.na(data$sulfate)))
  }
  
  data.frame(id, nobs)
}