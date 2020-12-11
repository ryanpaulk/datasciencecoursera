rankall <- function(outcome, num = "best") {
  ## Read outcome data
  excel <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomes <- c("Heart Attack", "Heart Failure", "Pneumonia")
  y = outcome %in% excel$outcomes
  if (y == FALSE) {
    stop("Invalid Outcome")
  }
  ##If cause of death (COD) is valid, direct data to specific column to use
  if (outcome == "Heart Attack") {
    death <- excel$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  if (outcome == "Heart Failure") {
    death <- excel$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  if (outcome == "Pneumonia") {
    death <- excel$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  ## Sort by lowest death 30 death rate
  ## Make Temporary Data Frame of State and CoD
  ## Include Hospital Name to Print later
  ## Treat COD as Numeric
  temp.df <- suppressWarnings(data.frame(excel$State, excel$Hospital.Name, as.numeric(death)))
  ## Make a For statement

  ordered.st <- st[order(st[,3], st[,2]),]
  
  dr <- c() ##empty vector
  for (i in 1:length(st)) {
    ##** I think rankhospital can be duplicated and then
    ##** produce a different output
  }
  ## For each state, find the hospital of the given rank
  ## For given outcome and rank, find the hospital and state
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}