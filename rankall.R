rankall <- function (outcome, num="best"){
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check if state is valid
    'if(!is.element(state, data$State)){
        stop("Invalid State")
    }'
    
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
    
    ## Substitute row with %death rate of outcome to numeric values
    outcomerow <- gsub("Not Available", NA, data[, colnumber])
    data[,colnumber] <- as.numeric(outcomerow)
    
    ## Generate list with a data frame for each state
    splitbystate <- split(data, data$State)
    
    ##to store the output
    result <- matrix(nrow=54, ncol=2)
    
    for (i in 1:length(splitbystate)){
        
        # Generate data frame for input state only
        statedata <- splitbystate[[i]]
        
        ## Re-order the state_data
        ### first by hospital name in ascending order
        ### then by death rate outcome in ascending order
        ranked_data <-  statedata[order(statedata[,colnumber],statedata[,2],na.last=NA),]
        
        ## accessing the top row of 7th column to get state name
        ## all other rows will have the same state name
        statename <- ranked_data[1,7]
        
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        if(is.numeric(num)==TRUE){
            newrowinresult <- c(ranked_data[num,2],statename)
            
        }
        
        if(num=="best"){
            newrowinresult <- c(ranked_data[1,2],statename) # creating rows for output matrix
            
        }
    
        if(num=="worst"){
            lastrow <- nrow(ranked_data)
            newrowinresult <- c(ranked_data[lastrow,2],statename)
            
        }
        
        ##inserting the rows created above to the 1st,2nd....ith columns of matrix
        result[i,] <- newrowinresult    
            
    }
    
    ##converting to get output as data frame
    rankedhospialsinstates <- data.frame(result)
    
    ##naming the rows as state names
    rownames(rankedhospialsinstates) <- result[,2] #to acess all the state names
    
    ##naming the columns
    names(rankedhospialsinstates) <- c("Hospital", "State") 
    
    rankedhospialsinstates
}
