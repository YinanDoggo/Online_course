
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


pairs(college[,c((ncol(college)-9):ncol(college))])
plot(Private,Grad.Rate)
rownames(subset(college,S.F.Ratio>25))


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


Question 9
```{sl ch2 q9}
Auto <- read.csv("AUto.csv", header=T, na.strings="?")
Auto <- na.omit(Auto)
colSums(is.na(Auto))
newAuto <- Auto[-c(10:85),]
sapply(Auto, function(x) sum(is.na(x)))


sapply(Auto[,1:7], range)
sapply(newAuto[,1:7], range)

f <- function(x){
        list(mean(x),sd(x))
        
}
sapply(Auto[,1:7],f)
sapply(newAuto[,1:7],f)

pairs(Auto)
```

Question 10
```{sl ch2 q10}
library(MASS)