best <- function (state,outcome){
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check if state is valid
    if(!is.element(state, data$State)){
        stop("Invalid State")
    }
    
    ## Check if outcome is valid
    possibleoutcome=c("heart attack","heart failure","pneumonia")
    if(!is.element(outcome, possibleoutcome)){
        stop("Invalid Outcome")
    }
    
    ## Get column number from file based on Outcome
    if(outcome=="heart attack"){
        colnumber <- 11
    }
    
    if(outcome=="heart failure"){
        colnumber <- 17
    }   
    
    if(outcome=="pneumonia"){
        colnumber <- 23
    }
    
    ##Generate list with a data frame for each state- Since Split always gives a list
    splitbystate <- split(data, data$State)
    
    ##Generate data frame for input state only
    statedata <- splitbystate[[state]]
    
    ##arranging the data frame in alphabetical order by hospital name
    statedata <- statedata[order(statedata[,2]),]
    
    
    ##Substitute "Not Available" in outcome row to NA to avoid coercion errors
    outcomerow <- gsub("Not Available", NA, statedata[, colnumber])
    
    ##index of first row with lowest death rate
    rownum <- which.min(outcomerow)
    
    
    ##extracting the hospital name in the rownum value, in the data frame for input state
    ##the alphabetical order is considered while extracting the name
    BestHospital <- statedata[rownum,2]
    
    BestHospital
    
}