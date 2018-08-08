rankall <- function(outcome, num){
        
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
        
        # check "function num"
        if(num > length(which(!is.na(data[,outcomeName])))){
                stop("NA")
        }
        
        ## For each state, find the hopsital of the given rank
        
        # get the list of state names (character)
        nameState <- levels(factor(rawdata[,7]))
        
        # split data according to state
        data <- split(data, data$State)
        
        # loop over list of state names
        result <- as.list(list)
        for(n in nameState){
                dataRanked <- data[[n]][order(data[[n]][,outcomeName], data[[n]][,"Hospital.Name"], na.last = NA),]
                #dataRanked <- data[[n]][num,]
                result <- rbind(result, dataRanked)
        }
        result
        result <- split(result, result$State)
        
        #result <- lapply(result, function(f) f[[1]][1:3])
        #result[[1]][1,]
        
        
        ## Return a data frame with the hospital names and the state name
        
}