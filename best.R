## From the hospital outcome data, return the best hospital based on state and 
## one of three outcomes (haeart attack, heart failure, or pneumonia)
best <- function(state, outcome) {
       ## Read outcome data
       outcomes <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
       hospitals <- read.csv("hospital-data.csv", colClasses = "character")
       
       ## Check that state and outcome are valid
       state_rows <- which(outcomes$State == state)
       if (length(state_rows) == 0) {
              stop("invalid state")
       }

       ## outcomes are one of “heart attack”, “heart failure”, or “pneumonia”.
       ## 11. Hospital 30-Day Death (Mortality) Rates from Heart Attack: 
       ##     Lists the risk adjusted rate
       ## 17. Hospital 30-Day Death (Mortality) Rates from Heart Failure: 
       ##     Lists the risk adjusted rate
       ## 23. Hospital 30-Day Death (Mortality) Rates from Pneumonia: 
       ##     Lists the risk adjusted rate
       # setup the highest (fake) values possible
       best_rate <- 101.0
       best_hosp <- "XXXXXXXXXXXXXXXXXXXXXXXXXX"
       # loop through the whole state list, throw out NAs, and find the best
       # rate and the hospital name that goes with it (sorted by hospital name)
       # all three are the same, just checking a different column
       if (outcome == "heart attack") {
               for (i in state_rows) {
                       check_this_hospital <- as.character(outcomes[i,2])
                       if (as.character(outcomes[i,11]) != "Not Available") {
                               check_this_rate <- as.numeric(as.character(outcomes[i,11]))
                               if (check_this_rate <= best_rate) {
                                       if (check_this_rate == best_rate) {
                                               if (best_hosp > check_this_hospital) {
                                                       best_hosp <- check_this_hospital
                                               }
                                       }
                                       best_rate <- check_this_rate
                                       best_hosp <- check_this_hospital
                               }
                       }
               }                
       } else if (outcome == "heart failure") {
                      for (i in state_rows) {
                              check_this_hospital <- as.character(outcomes[i,2])
                              if (as.character(outcomes[i,17]) != "Not Available") {
                                      check_this_rate <- as.numeric(as.character(outcomes[i,17]))
                                      if (check_this_rate <= best_rate) {
                                               if (check_this_rate == best_rate) {
                                                       if (best_hosp > check_this_hospital) {
                                                               best_hosp <- check_this_hospital
                                                       }
                                               }
                                       best_rate <- check_this_rate
                                       best_hosp <- check_this_hospital
                                       }
                              }
                      }
       } else if (outcome == "pneumonia") {
                       for (i in state_rows) {
                       check_this_hospital <- as.character(outcomes[i,2])
                       if (as.character(outcomes[i,23]) != "Not Available") {
                               check_this_rate <- as.numeric(as.character(outcomes[i,23]))
                               if (check_this_rate <= best_rate) {
                                       if (check_this_rate == best_rate) {
                                               if (best_hosp > check_this_hospital) {
                                                       best_hosp <- check_this_hospital
                                               }
                                       }
                                       best_rate <- check_this_rate
                                       best_hosp <- check_this_hospital
                               }
                       }
               } 
       } else {
              stop("invalid outcome")
       }
       
       ## Return hospital name in that state with lowest 30-day death rate
       best_hosp
}

