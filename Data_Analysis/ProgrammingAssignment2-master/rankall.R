rankall <- function(outcome, num = "best"){
        
## Read outcome data
        
        # read in rawdata
        rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        # outcome definition
        outcomeName <- as.character()
        if(outcome == "heart attack"){
                outcomeName = names(rawdata[11])
        } else if(outcome == "heart failure") {
                outcomeName = names(rawdata[17])
        } else if(outcome == "pneumonia"){
                outcomeName = names(rawdata[23])
        }
        
        # subset rawdata 
        data <- rawdata[,c("State","Hospital.Name",outcomeName)]
        # make outcome data numeric
        data[,outcomeName] <- as.numeric(data[,outcomeName])
        
## Check that outcome and ranking are valid
        
        # check "function outcome"
        if(!(outcome %in% c("heart attack", "heart failure", "pneumonia"))){
                stop("invalid outcome")
        }
        ## For each state, find the hopsital of the given rank
        
        # get the list of state names (character)
        nameState <- levels(factor(rawdata[,7]))
        
        # split data according to state
        data <- split(data, data$State)
        
        # define an empty data frame to store result 
        final <- data.frame()
        
        # loop over list of state names
        for(n in nameState){
        #n <- "AL"
                
                # rank data according to "function outcome" and then hospital name
                dataRanked <- data[[n]][order(data[[n]][,outcomeName], data[[n]][,"Hospital.Name"], na.last = NA),]
                
                # the last position of the available data for "function outcome"
                #Upperbound <- length(dataRanked[,outcomeName])
                Upperbound <- length(dataRanked[,outcomeName])
                
                # check if the "function num" is larger than available elements
                #if(num > Upperbound){
                #        NA
                #}
                       
                # check "function num" if equal to "worst" and "best"
                if(num == "worst"){
                        x = Upperbound
                        result <- dataRanked[x,]
                } else if(num == "best"){
                        num = 1
                        result <- dataRanked[num,]
                } else if(num != "worst" & num != "best"){
                        num = num
                        result <- dataRanked[num,]
                }
        
                
                final <- rbind(final, result)
        }
        
        final
        #dataRanked
        #Upperbound
        
        #length(dataRanked$State)
        #esult <- split(result, result$State)
        
        #result <- lapply(result, function(f) f[[1]][1:3])
        #result[[1]][1,]
       #dataRanked
        
        
## Return a data frame with the hospital names and the state name
        
}

r <- rankall("heart attack", 4)
as.character(subset(r, state == "HI")$hospital)