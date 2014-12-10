complete <- function(directory, id = 1:332) {
  ids <- c()
  nobs <- c()
  for (filePrefix in id) {
    filePath <- paste(directory, getFileName(filePrefix), sep = '/')
    data <- read.csv(filePath)
    goodSulfate <- !is.na(data['sulfate'][,1])
    goodNitrate <- !is.na(data['nitrate'][,1])
    completeEntries <- sum(goodNitrate * goodSulfate)
    ids <- c(ids, filePrefix)
    nobs <- c(nobs, completeEntries)
  }
  data.frame(id = ids, nobs = nobs)
}

getFileName <- function(filePrefix) {
  if (filePrefix < 10) {
    return (paste("00", filePrefix, ".csv", sep = ''))
  } else if (filePrefix < 100) {
    return (paste("0", filePrefix, ".csv", sep = ''))
  }
  return (paste(filePrefix, ".csv", sep = ''))
}
