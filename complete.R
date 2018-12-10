complete <- function(directory, id=1:332){
    
    nobs <- numeric()
    
    for (i in id){
        
        data <- read.csv(filelist[[i]])
        nobs <- c(nobs,sum(complete.cases(data)))
    }
    dataframe <- data.frame(id,nobs)
    
    dataframe
    
}
