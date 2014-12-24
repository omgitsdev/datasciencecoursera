rankhospital <- function(state, outcome, num = "best") {
       ## Read outcome data
  outcomes <- read.csv('outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
  if (!is.element(tolower(state), tolower(outcomes$State))) stop("invalid state")
  if (tolower(outcome) == "heart attack") {
    outcomeCol <- 11
    outcomes[,11] <- as.numeric(as.character(outcomes[,11]))
  } else if (tolower(outcome) == "heart failure") {
    outcomeCol <- 17
    outcomes[,17] <- as.numeric(as.character(outcomes[,17]))
  } else if (tolower(outcome) == "pneumonia") {
    outcomeCol <- 23
    outcomes[,23] <- as.numeric(as.character(outcomes[,23]))
  } else {
    stop('invalid outcome')
  }
  ## Return hospital name in that state with given rank
  s <- outcomes[outcomes$State == toupper(state), ]
  s <- s[complete.cases(s[, outcomeCol]),]
  ordered <- s[order(s[[outcomeCol]], s$Hospital.Name), ]
  if (num == "best") num <- 1
  if (num == "worst") num <- nrow(ordered)
  if (num > nrow(ordered)) return(NA)
  return (as.character(ordered[[2]][num]))
}
