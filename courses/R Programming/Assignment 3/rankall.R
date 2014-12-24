best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv('outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
  if (!is.element(tolower(state), tolower(outcomes$State))) stop("invalid state")
  if (!is.element(tolower(outcome), c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death

  ## rate
  5
}

rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
}

rankall <- function(outcome, num = "best") {
        ## Read outcome data
        ## Check that state and outcome are valid
        ## For each state, find the hospital of the given rank
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
}
