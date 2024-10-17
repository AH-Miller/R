corr <- function(directory, threshold = 0) {
## 'directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations
  
  num_files <- length(list.files(directory))
  cors <- c()
  comp <- complete(directory, 1:num_files)
  for (i in 1:num_files) {
    if (comp$nobs[comp$id == i] >= threshold) {
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
      data_cor <- cor(data$sulfate, data$nitrate, use = "na.or.complete")
      if (!is.na(data_cor)) {
        cors <- c(cors, data_cor)
      }
    }
  }
  cors
}