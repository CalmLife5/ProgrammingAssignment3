# Best, Worst, or what rank (5th, or 10th - like that)
rankhospital <- function(state, outcome, num = "best") {

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
       
        ## Check that state and outcome are valid
        state_rows <- which(outcomes$State == state)
        if (length(state_rows) == 0) {
               stop("invalid state")
        }
        
        best_hosp <- 0
        if (class(num) == "character") {
                if (!((num == "best") | (num == "worst"))) {
                        best_hosp <- NA
                }
        } else if (class(num) == "numeric") {
                       if ((num < 0) | (num > length(state_rows))) {
                               best_hosp <- NA
                       }
        }
                
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
               
        }       
        ## Return hospital name in that state with lowest 30-day death rate
        best_hosp
}