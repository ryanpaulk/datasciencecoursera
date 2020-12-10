best <- function(state, outcome) {
  ## Read outcome data
  excel <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  ## is "state" found in Column = "State"?
  x = state %in% excel$State
  ## is cause of death one of Heart Failure, Attack, or Pneumonia?
  outcomes <- c("Heart Failure", "Heart Attack", "Pneumonia")
  y = outcome %in% outcomes
  if (x == FALSE) {
    stop("Invalid State")
  }
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
  suppressWarnings(temp.df <- data.frame(excel$State, excel$Hospital.Name, as.numeric(death)))
  ## Return hospital name in that state with lowest 30-day death
  st <- subset(temp.df, excel$State == state) ## only rows of requested state
  ## death rate
  dr <- c()
  for (i in 1:length(st)) {
    dr <- st[i] == min((st[, 3]), na.rm = TRUE) ## 3 is column = death
}
result <- data.frame(st[, 2], dr) ## Hospital Name and min CoD
result2 <- subset(result, result[,2] == TRUE)
return(result2)
}
##alphabetize

## delete later: 
## outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")