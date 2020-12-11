rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  excel <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  x = state %in% excel$State
  outcomes <- c("Heart Attack", "Heart Failure", "Pneumonia")
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
  ## Return hospital name in that state with the given rank
  st <- temp.df[excel$State == state,] ## create a subset of only that state
  ## death rate organization
  dr <- c()
  for (i in 1:length(st)) {
    dr <- st[i] == min((st[, 3]), na.rm = TRUE) ## 3 is column = death
  }
  result <- data.frame(st[, 2], dr) ## Hospital Name and min CoD
  result2 <- subset(result, result[,2] == TRUE)
  ##alphabetize
  alpha <- result2[order(result2$st...2.),]
  if (num == "best") {
    alpha[1,2]
  } else if (num == "worst") {
    alpha[nrow(alpha), 2]
  } else {
    alpha[num, 2]
  }
}
  ## 30-day death rate