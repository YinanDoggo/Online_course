rankhospital <- function(state, outcome, num = "best") {
        
#Read outcome data
        
        #read the whole table in
        rawdata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        #define the outcomeName to the column number of the rawdata according to the
        #"function outcome"
        ChkList <- c("heart attack", "heart failure", "pneumonia")
        outcomeName <- as.character()
        if(outcome == ChkList[1]){
                outcomeName = names(rawdata)[11]
        } else if (outcome == ChkList[2]){
                outcomeName = names(rawdata)[17]
        } else if (outcome == ChkList[3]){
                outcomeName = names(rawdata)[23]
        }
        
        #subset the table where the column "State" equal to the "function state"
        data <- rawdata[rawdata["State"] == state, ]
        
        #subset the table according the "function outcome"
        data <- data[,c("State","Hospital.Name",outcomeName)]
        
        #define the upper bound for the maximal number of hospitals to be ranked
        UpperBound <- length(data$Hospital.Name)
        
        #Check whether "function state" and "function outcome" are valid
        data[,outcomeName] <- as.numeric(data[,outcomeName])
        if(!(state %in% rawdata[,"State"])){
                stop("invalid state")
        }
        if(!(outcome %in% ChkList)){
                stop("invalid outcome")
        }
        if(num == "best"){
                num = 1
        } else if(num == "worst"){
                num = length(which(!is.na(data[,outcomeName])))       
        } else if(num > UpperBound) {
                stop("NA")
        }
    
        #Return hospital name in that state with lowest 30-day death
        #data[,outcomeName] <- as.numeric(data[,outcomeName])
        result <- data[order(data[ ,outcomeName], data[ ,"Hospital.Name"], na.last = NA), ]
        result[num,]
}