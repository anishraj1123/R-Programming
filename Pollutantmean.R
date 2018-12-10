pollutantmean <- function(directory, pollutant, id=1:332){
    
    filelist <- list.files("specdata", pattern=".csv", full.names=TRUE)
    means <- numeric()
    
    for (i in id){
        data <- read.csv(filelist[i])
        means <- c(means, data[[pollutant]])
    }
    finalmean <- mean(means, na.rm=TRUE)
    finalmean
}