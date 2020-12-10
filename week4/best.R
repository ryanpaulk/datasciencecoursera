best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## is "state" found in Column = "State"?
  x = state %in% outcome$State
  ## is cause of death one of Heart Failure, Attack, or Pneumonia?
  y = outcome %in% c("Heart Failure", "Heart Attack", "Pneumonia")
  if (x == FALSE) {
    stop("Invalid State")
  }
  if (y == FALSE) {
    stop("Invalid Outcome")
  }
  ##If cause of death is valid, direct data to specific column to use
  if (outcome == "Heart Attack") {
    death <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack
  }
  if (outcome == "Heart Failure") {
    death <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure
  }
  if (outcome == "Pneumonia") {
    death <- outcome$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia
  }
  }
  }
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}

## delete later: 
## outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")