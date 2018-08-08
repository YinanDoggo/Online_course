pollutantmean <- function(directory, pollutant, id = 1:332) 
        {
  
        file = list.files(directory, full.names=TRUE)
        mydata <- data.frame()
        for(i in id) 
                {
                mydata <- rbind(mydata, read.csv(file[i]))
        }

        p_mean <- mean(mydata[,pollutant], na.rm = TRUE)
        round(p_mean, digits = 3)
  
}

complete <- function(directory, id = 1:332) {
        
        file <- list.files(directory,full.names=TRUE)
        Result <- numeric()
         
        for (i in id) 
                
        {
                mydata <- read.csv(file[i])
                SumRow <- nrow(mydata[complete.cases(mydata),])
                Result <- c(Result, SumRow)
                
        }
        
        return(data.frame("id"=id, "nobs"=Result))

}


corr <- function(directory, threshold = 0) {
        
        file <- list.files(directory,full.names=TRUE)
        List <- vector()
        
        for (i in 1:332) 
                
        {
                mydata <- read.csv(file[i])
                x <- complete(directory,i)
                
                if (x[,"nobs"] > threshold) {
                        y <- cor(mydata[,"sulfate"],mydata[,"nitrate"], use = "pairwise.complete.obs")
                        List <- c(List,y)
                }
                
                else {next}
         
        }
        
        print(List)
}