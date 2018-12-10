corr <- function(directory, threshold=0){
    
    corrsnum <- numeric(0)
    filelist1 <- list.files("specdata", pattern=".csv", full.names=TRUE)
    
    value <- complete("specdata",1:332)
    value <- value[value$nobs>threshold,]
    
    for (i in value$id){
        data1 <- read.csv(filelist1[[i]])
        corrsnum <- c(corrsnum, cor(data1$sulfate, data1$nitrate, use="complete.obs"))
        }
    'corrsnum1 <- data.frame(corrsnum)'
     corrsnum
        
     
}
