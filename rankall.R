rankall <- function(outcome, num = "best") {

## Check that state and outcome are valid
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the
## (abbreviated) state name

## Read outcome data
        xxxGetRanked <- function(whatcol) {
               oneState <- subset(outcomes, State == state)
               justCheck <- oneState[,c(2,whatcol)]
               justCheck[,2] <- as.numeric(justCheck[,2])
               row.has.na <- apply(justCheck, 1, function(x){any(is.na(x))})
               justCheck <- justCheck[!row.has.na,]
               justCheck <- justCheck[order(justCheck[,2],justCheck[,1]),]
               justCheck
        }
      
        ## Read outcome data
        outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
       
        best_hosp <- 0
        if (class(num) == "character") {
                if (!((num == "best") | (num == "worst"))) {
                        best_hosp <- NA
                }
        }
        
        stateList <- c("AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", "GU", "HI", "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", "NM", "NV", "NY", "OH", "OK", "OR", "PA", "PR", "RI", "SC", "SD", "TN", "TX", "UT", "VA", "VI", "VT", "WA", "WI", "WV", "WY")
        results <- data.frame(matrix(nrow = 54, ncol = 2))
        colnames(results) <- c("hospital", "state")
        rownames(results) <- stateList
                
        stateCtr <- 0        
        if (!is.na(best_hosp)) {
               ## outcomes are one of “heart attack”, “heart failure”, or “pneumonia”.
               ## 11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
               ##     Lists the risk adjusted rate
               ## 17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: 
               ##     Lists the risk adjusted rate
               ## 23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
               ##     Lists the risk adjusted rate
         
               # loop through the whole state list, throw out NAs, and find the best
               # rate and the hospital name that goes with it (sorted by hospital name)
               # all three are the same, just checking a different column
 
               for (state in stateList) { 

                 if (outcome == "heart attack") {
                         best_host <- xxxGetRanked(11)
                 } else if (outcome == "heart failure") {
                                best_host <- xxxGetRanked(17)
                 } else if (outcome == "pneumonia") {
                                best_host <- xxxGetRanked(23)
                 } else {
                        stop("invalid outcome")
                 }    

                 if (class(num) == "character") {
                         if (num == "best") {
                                 best_hosp <- best_host[1, 1]
                         } else if (num == "worst") {
                                 best_host <- tail(best_host, 1)
                                 best_hosp <- best_host[1, 1]
                         } 
                 } else {
                        best_hosp <- best_host[num,1]
                 }
                 stateCtr <- stateCtr + 1
                 results[stateCtr, 1] <- best_hosp
                 results[stateCtr, 2] <- state
                 
               }        
               
        }       
        ## Return hospital name in that state with lowest 30-day death rate
        results
}
