best <- function(state, outcome) {
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
  ## Return hospital name in that state with lowest 30-day death
  s <- outcomes[outcomes$State == toupper(state), ]
  ordered <- s[order(s[[outcomeCol]], s$Hospital.Name), ]
  return (as.character(ordered[[2]][1]))
}
