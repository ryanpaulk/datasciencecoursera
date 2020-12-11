rankall <- function(outcome, num = "best") {
  ## Read outcome data
  excel <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  outcomes <- c("Heart Attack", "Heart Failure", "Pneumonia")
  y = outcome %in% outcomes
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

  ordered.df <- temp.df[order(temp.df[,3], temp.df[,2], na.last = NA),] ##Omit the incomplete
  
  output <- vector() ##empty vector
  states <- levels(excel$State) ## not sure why ##

  for (i in 1:length(ordered.df)) {
    
    hospital <- if (num == "best") {
    ordered.df[1,2]
  } else if (num == "worst") {
    ordered.df[nrow(ordered.df), 2]
  } else {
    ordered.df[num, 2]
  }
  output <- append(output, c(hospital, states[i]))
    ##** I think rankhospital can be duplicated and then
    ##** produce a different output
  }
  output <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
  colnames(output) <- c("Hospital", "State")
  rownames(output) <- states
  output
  ## For each state, find the hospital of the given rank
  ## For given outcome and rank, find the hospital and state
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}