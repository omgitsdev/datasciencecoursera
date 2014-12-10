pollutantmean <- function(directory, pollutant, id = 1:332) {
  runningMean <- 0
  totalEntries <- 0
  for(filePrefix in id) {
    filePath <- paste(directory, getFileName(filePrefix), sep = '/')
    data <- read.csv(filePath)
    fileMean <- mean(data[pollutant][,1], na.rm = T)
    validEntries <- sum(!is.na(data[pollutant][,1]))
    if (validEntries == 0) {
      fileMean <- 0
    }
    runningMean <- (runningMean * totalEntries + fileMean * validEntries) / (totalEntries + validEntries)
    totalEntries <- totalEntries + validEntries
  }
  runningMean
}

getFileName <- function(filePrefix) {
  if (filePrefix < 10) {
    return (paste("00", filePrefix, ".csv", sep = ''))
  } else if (filePrefix < 100) {
    return (paste("0", filePrefix, ".csv", sep = ''))
  }
  return (paste(filePrefix, ".csv", sep = ''))
}
