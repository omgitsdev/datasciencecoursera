pollutantmean <- function(directory, pollutant, id = 1:332) {
  fileNames <- paste(directory, '/', sprintf("%03d.csv", id), sep = '')
  dataSet <- rbind(read.csv(fileNames))ta
}