best <- function(state, outcome) {
  ## Read outcome data
  outcomes <- read.csv('outcome-of-care-measures.csv')
  ## Check that state and outcome are valid
  if (!is.element(tolower(state), tolower(outcomes$State))) stop("invalid state")
  if (!is.element(tolower(outcome), c("heart attack", "heart failure", "pneumonia"))) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death

}
