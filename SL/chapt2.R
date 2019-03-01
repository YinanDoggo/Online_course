
library(ggplot2)
library(dplyr)
library(tidyr)

college <- read.csv("College.csv")

#fix(college)
rownames(college)=college[,1]
#fix(college)
college=college[,-1]
#fix(college)

summary(college)
attach(college)


pairs(college[,c(2,3,4,7,8)])
plot(Private,Room.Board)
rownames(college)[which(college$Apps > 40000)]


Elite=rep("No",nrow(college))
Elite[Top10perc>50]="Yes"
Elite=as.factor(Elite)
college=data.frame(college,Elite)

summary(college)

par(mfrow=c(1,1))
hist(Enroll)
hist(Apps)
hist(Accept)
hist(PhD)
