rankstate <- function(state, s, num, outcomeCol) {
  ordered <- s[[state]][order(s[[state]][[outcomeCol]], s[[state]]$Hospital.Name), ]
  if (num == "best") num <- 1
  if (num == "worst") num <- nrow(ordered)
  # worstOutcome <- ordered[[outcomeCol]][num]
  if (num > nrow(ordered)) return(NA)
  return (as.character(ordered[[2]][num]))
}

rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomes <- read.csv('outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
  if (tolower(outcome) == "heart attack") {
    outcomeCol <- 11
    # outcomes[,11] <- as.numeric(as.character(outcomes[,11]))
  } else if (tolower(outcome) == "heart failure") {
    outcomeCol <- 17
    # outcomes[,17] <- as.numeric(as.character(outcomes[,17]))
  } else if (tolower(outcome) == "pneumonia") {
    outcomeCol <- 23
    # outcomes[,23] <- as.numeric(as.character(outcomes[,23]))
  } else {
    stop('invalid outcome')
  }
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  #remove all NA rows for given outcome
  outcomes[,outcomeCol] <- as.numeric(as.character(outcomes[,outcomeCol]))
  outcomes <- outcomes[complete.cases(outcomes[, outcomeCol]),]
  s <- split(outcomes, outcomes$State)
  state <- names(s)
  hospital <- sapply(state, rankstate, s, num, outcomeCol)
  return (data.frame(hospital, state))
}
