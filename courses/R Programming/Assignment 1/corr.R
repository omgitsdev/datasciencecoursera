source('complete.R')

corr <- function(directory, threshold = 0) {
  cr <- c()

  fileNames <- dir(directory)
  for (i in fileNames) {
    if (complete(directory, as.integer(substr(i, 1, 3)))$nobs > threshold) {
      good <- na.omit(read.csv(paste(directory, '/', i, sep = '')))
      cr <- c(cr, cor(good$sulfate, good$nitrate))
    }
  }
  return (cr)
}
