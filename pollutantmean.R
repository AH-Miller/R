pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the 
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result!
  
  measurements <- NULL
  
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
    
    if (pollutant == "sulfate") {
      measurements <- c(measurements, data$sulfate)
    }
    else if (pollutant == "nitrate") {
      measurements <- c(measurements, data$nitrate)
    }
  }
  mean(measurements, na.rm = TRUE)    
}