
# Q1 - Textbook Solution
if(!file.exists("quiz1")){
        dir.create("quiz1")
}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
download.file(fileUrl, destfile = "./data/realestate.csv", method = "curl")
list.files("./quiz1")
dateDownloaded <- date()
df1 <- read.csv("realestate.csv", header = TRUE, sep = ",")
Quiz1Answer1 <- nrow(subset(df1, VAL==24))
Quiz1Answer1

# Q1 - My Solution
if(!file.exists("quiz1")){
        dir.create("quiz1")
}
library("data.table")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "IdahoHousing.csv")
df1 <- fread("IdahoHousing.csv")
Quiz1Answer1  <- nrow(df1[df$VAL=24,])
Quiz1Answer1

# Q3 - My Solution
if(!file.exists("quiz1")){
        dir.create("quiz1")
}
df3 <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FDATA.gov_NGAP.xlsx", "NGAP.xlsx")
# problem with read xlsx again because of JAVA

# Q4 - My Solution
library(bitops)
library(RCurl)
library(XML)
fileUrl <- download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", "FRes.xml")
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
rootNode <- xmlRoot(doc)
xmlName(rootNode)
names(rootNode)
zipcode <- xpathSApply(rootNode, "//zipcode", xmlValue)
zipcode
sum(zipcode == 21231)

# Q5 - My Solution
library("data.table")


if(!file.exists("quiz1")){
        dir.create("quiz1")
}
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", "IdahoHousing.csv")
DT <- fread("IdahoHousing.csv")

# 1
start.time <- Sys.time()
pwgtp15 <- DT[,mean(pwgtp15),by=SEX]
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 2
start.time <- Sys.time()
pwgtp15 <- rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 3
start.time <- Sys.time()
pwgtp15 <- mean(DT$pwgtp15,by=DT$SEX)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 4
start.time <- Sys.time()
pwgtp15 <- mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 5
start.time <- Sys.time()
pwgtp15 <- tapply(DT$pwgtp15,DT$SEX,mean)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# 6
start.time <- Sys.time()
pwgtp15 <- sapply(split(DT$pwgtp15,DT$SEX),mean)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
