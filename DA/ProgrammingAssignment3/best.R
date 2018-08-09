best <- function(state, outcome) {
        #Read outcome data
        
        rawdata <- subset(read.csv("outcome-of-care-measures.csv", colClasses = "character"), state == State)
        
        OutcomeList <- list("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
        rawdata[,11] <- as.numeric(rawdata[,11])
        rawdata[,17] <- as.numeric(rawdata[,17])
        rawdata[,23] <- as.numeric(rawdata[,23])
        ChkList <- c("heart attack", "heart failure", "pneumonia")

        
        #Check that state and outcome are valid
        
        if(!(state %in% rawdata[,7])){
                stop("invalid state")
        }
        if(!(outcome %in% ChkList)){
                stop("invalid outcome")
        }
       
        #Return hospital name in that state with lowest 30-day death
        
        if(outcome == "heart attack"){
        Position1 <- which.min(rank(rawdata[,11]))
        HosName1 <- rawdata$Hospital.Name[Position1]
        c(state, HosName1)
        }
        
        else if(outcome == "heart failure"){
                #Return hospital name in that state with lowest 30-day death
                Position2 <- which.min(rank(rawdata[,17]))
                HosName2 <- rawdata$Hospital.Name[Position2]
                c(state, HosName2)
        }
        
        else if(outcome == "pneumonia"){
                #Return hospital name in that state with lowest 30-day death
                Position3 <- which.min(rank(rawdata[,23]))
                HosName3 <- rawdata$Hospital.Name[Position3]
                c(state, HosName3)
        }
    
}