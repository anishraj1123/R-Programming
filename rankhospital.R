rankhospital <- function (state,outcome, num="best"){
    
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
    if(outcome =="heart attack"){
        colnumber <- 11
    }
    
    if(outcome =="heart failure"){
        colnumber <- 17
    }   
    
    if(outcome =="pneumonia"){
        colnumber <- 23
    }
    
    ##Generate list with a data frame for each state- Since Split always gives a list
    splitbystate <- split(data, data$State)
    
    ## Generate data frame for input state only
    statedata <- splitbystate[[state]]
    
    ## Substitute "Not Available" in outcome row to NA to avoid coercion errors
    outcomerow <- gsub("Not Available", NA, statedata[, colnumber])
    
    ## ## Substitute row with %death rate of outcome to numeric values
    statedata[,colnumber] <- as.numeric(outcomerow)
    
    
    ## Re-order the statedata 
    ### first by hospital name in ascending order 
    ### then by death rate outcome in ascending orde
    ranked_data <-  statedata[order(statedata[,colnumber],statedata[,2],na.last=NA),]
    
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    if(is.numeric(num)==TRUE){
        rankedhospital <- ranked_data[num,2] # accessing the 2nd Column(Hospital name)
        
    }
    
    if(num == "best"){
        rankedhospital <- ranked_data[1,2]
        
    }
    
    if(num == "worst"){
        num=nrow(ranked_data)
        rankedhospital <- ranked_data[num,2]
        
    }
    
    rankedhospital
    
}
