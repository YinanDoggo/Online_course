regplot <- function(x,y,...){
        fit <- lm(y~x)
        plot(x,y,...)
        abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)

attach(Auto) ; summary(lmauto <- lm(mpg~horsepower)) ; par(mfrow=c(2,2)) ; plot(lmauto)

predict(lmauto,data.frame(horsepower=c(98)),interval="confidence")
predict(lmauto,data.frame(horsepower=c(98)),interval="prediction")
par(mfrow=c(2,2))

plot(mpg~horsepower)
abline(lmauto,lwd=3,col="red")
plot(predict(lmauto))
plot(residuals(lmauto))
plot(predict(lmauto),rstudent(lmauto))

pairs(Auto)
View(cor(Auto[,-9]))
summary(lmauot2 <- lm(mpg~.-name,data=Auto))
lmauto2
plot(lmauto2)

lmauto3 <- lm(mpg ~
                      +displacement*cylinders 
              + displacement*weight
              #+ cylinders*horsepower 
              #+ horsepower*displacement 
              #+ horsepower*weight 
              ,data=Auto)
plot(lmauto3) ; summary(lmauto3)

par(mfrow=c(2,2))
lmauto4 <- lm(mpg~.
              -acceleration
              -name-weight
              -cylinders
              +displacement*weight
              +poly(year,3)
              +year:horsepower
              ,data=Auto) 
lmauto4 <- lm(log(mpg)~cylinders+displacement+horsepower+weight+acceleration+year+origin,data=Auto)
plot(lmauto4)
summary(lmauto4)
anova(lmauto2,lmauto4)

names(Carseats)
lmcar <- lm(Sales~Price+Urban+US) ; summary(lmcar)
?Carseats
summary(Carseats)
lmcar2 <- lm(Sales~Price+US) ; summary(lmcar2) ; plot(lmcar2)
confint(lmcar2)
plot(predict(lmcar2),rstudent(lmcar2))


set.seed(1)
x = rnorm(100)
y = 2*x + rnorm(100)
lmfit <- lm(y ~ 0 + x) ; summary(lmfit) ; plot(lmfit)
lmfit2 <- lm(x ~ 0 + y) ; summary(lmfit2) ; plot(lmfit2)

# Question 12
# a) when they are perfectly correlated and has a slope of 0.5
# b)
set.seed(1)
x = rnorm(100)
y = rnomr(100)
lmfit1 <- lm(y~0+x) ; summary(lmfit1) ; plot(lmfit1)
lmfit2 <- lm(x~0+y) ; summary(lmfit2) ; plot(lmfit2)
# c)
x <- rnorm(100)
y <- sample(x,100,replace = FALSE)
lmfit1 <- lm(y~0+x) ; summary(lmfit1) ; plot(lmfit1)
lmfit2 <- lm(x~0+y) ; summary(lmfit2) ; plot(lmfit2)
boxplot(x)
boxplot(y)

# Question 13
# a) 
set.seed(1)
x <- rnorm(100,0,5)
# b)
e <- rnorm(100,0,5)
# c) d)
y <- -1 + 0.5*x + e ; plot(x,y)
# e)
lmfit <- lm(y~x); summary(lmfit); plot(lmfit)
# f)
par(mfrow=c(1,1))
plot(x,y) ; abline(lmfit,col="red") ; abline(-1,0.4,col="green")
legend(0.5,10, legend = c("model fit", "pop. regression"), col = c("red","green"), lwd = 3)
# g)
par(mfrow=c(2,2))
lmfit2 <- lm(y~x+I(x^2)); plot(lmfit2) ; summary(lmfit2)
anova(lmfit,lmfit2)
summary(lmfit)
# h)
#e <- rnorm(100,0,125) # then repeat above
# i)
#e <- rnorm(100,0,5) # then repeat above
# j)
con3 <- confint(lmfit)
con1 ; con2 ; con3
con1[2,2] - con1[2,1] ; con2[2,2] - con2[2,1] ; con3[2,2] - con3[2,1]

# Question 14
# a)
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)
# b)
cor(x1,x2); plot(x1,x2)
# c)
lmfit <- lm(y~x1+x2); plot(lmfit); summary(lmfit)
# d)
lmfit2 <- lm(y~x1); plot(lmfit2); summary(lmfit2)
# e)
lmfit3 <- lm(y~x2); plot(lmfit3); summary(lmfit3)
# f) No. They are correlated.
# g)
x1 <- c(x1, 0.1) ; x2 <- c(x2, 0.8) ; y <- c(y, 6)
plot(predict(lmfit),rstudent(lmfit)) ; plot(predict(lmfit2),rstudent(lmfit2)) ; plot(predict(lmfit3),rstudent(lmfit3)) 

# Question 15

# a)
LoadLibraries = function(){
        library(MASS)
        library(ISLR)
        print("lock and load")
}
LoadLibraries()
par(mfrow=c(2,2))
result <- lapply(1:length(Boston), function(x) summary(lm(crim~Boston[,x])))
View(result)
lapply(1:length(Boston), function(x) plot(result[x]))

# b)
lmfit2 <- lm(crim~., data=Boston) ; summary(lmfit2) ; plot(lmfit2)
plot(predict(lmfit2), rstudent(lmfit2))

# c)
coef.single <- sapply(2:length(Boston), function(x) coefficients(lm(crim~Boston[,x]))[2])
coef.all <- coefficients(lmfit2)
par(mfrow=c(2,2))
plot(coef.single,coef.all[-1]) ; plot(coef.single) ; plot(coef.all[-1])
View(coef.all)

# d)
par(mfrow=c(2,2))
lm.sum <- lapply(1:length(Boston), function(x) lm(crim~poly(Boston[,x], 3, raw = TRUE)))
lm.sum <- lapply(1:length(Boston), function(x) lm(crim~Boston[,x]+I(Boston[,x]^2)+I(Boston[,x]^3)))
names(lm.sum) <- names(Boston)[1:14]
lapply(lm.sum, plot) ; lapply(lm.sum, summary)
lm.medv <- (lm(crim~poly(medv,3)))

par(mfrow=c(1,1))
plot(crim~medv)
points(medv, fitted(lm.medv),col="red",lwd=3)

# Chapter 4 Exercise

# Question 6
exp(-6+0.05*40+1)/(1+exp(-6+0.05*40+1)) #a
(log(1)+6-3.5)/0.05 #b

# Question 7
4*10/36 - (10^2)/(2*36) + log(0.8)

# chapter 4
# QUestion 10
library(ISLR)
library(MASS)
?Weekly
summary(Weekly)
attach(Weekly)
head(Weekly)
names(Weekly)


# a)
cor(Weekly[,-9]) ; pairs(Weekly, col=Direction) # no significant pattern except between Year and Volume

# b)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, family=binomial, data=Weekly)
summary(glm.fit) # intercept and Lag2 are significant

# c)
glm.prob = predict(glm.fit, type="response")
head(glm.prob)
head(Weekly)

        # what predict(glm.fit, type="response") really do: 
        # probability = exp(linear function f) / (1 + exp(linear function f))
        # linear function f use the coefficients from the logistic regression 
x <- c(1, Lag1[1],Lag2[1],Lag3[1],Lag4[1],Lag5[1],Volume[1])
b <- c(coef(glm.fit))
f <- sum(x*b)
exp(f)/(1+exp(f)) # same as glm.pprb = predict(glm.fit, type="response")

glm.pred = rep("Down",nrow(Weekly))
glm.pred[glm.prob>0.5] = "Up"

table(glm.pred, Direction)
1 - (54+557)/(48+430+54+557) # training error rate = sum of correct divided by total number of observations
1 - mean(glm.pred == Weekly$Direction) # training error rate

# d)
train = Year<2009
length(train)
glm.fit = glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)
summary(glm.fit)
glm.prob = predict(glm.fit, newdata=Weekly[!train,], type="response")
glm.pred = ifelse(glm.prob>0.5, "Up","Down")
table(glm.pred, Direction[!train])
mean(glm.pred==Direction[!train])

# e)
lda.fit = lda(Direction~Lag2, data=Weekly, subset=train)
lda.pred = predict(lda.fit, newdata=Weekly[!train,])
table(lda.pred$class, Weekly$Direction[!train])
mean(lda.pred$class==Direction[!train]) # 0.625 corret predicition rate
#         Down Up
#  Down    9   5
#  Up     34   56

# f)
qda.fit = qda(Direction~Lag2, data=Weekly, subset=train)
qda.pred = predict(qda.fit, newdata=Weekly[!train,])
table(qda.pred$class, Direction[!train])
mean(qda.pred$class==Direction[!train]) # 0.586 corect prediction rate
        # down up
#  Down   0    0
#  Up     43   61

# g)
library(class)
set.seed(1)
knn.pred = knn(as.matrix(Lag2[train]), as.matrix(Lag2[!train]), Weekly$Direction[train], k=1)
table(knn.pred, Direction[!train])
mean(knn.pred==Direction[!train])

train = Year<2009
set.seed(1)
knn.train = knn(as.matrix(Lag2[train]), as.matrix(Lag2[train]), Weekly$Direction[train], k=1)
table(knn.train, Direction[train])
#knn.pred  Down  Up
#   Down   21    30
#   Up     22    31

#   0.5 correct prediction rate

# trial to calculate LD1 and posterior probability "by hand" based on e)
test.down <- subset(Weekly[,"Lag2"],Direction=="Down" & Year<2009)
test.up <- subset(Weekly[,"Lag2"],Direction=="Up" & Year<2009)
test <- subset(Weekly[,"Lag2"], Year<2009)
length(test.down) ; length(test.up); length(test)
mean(test.down) ; mean(test.up) ; var.test <- (var(test.down) + var(test.up))*length(test)/(length(test)-2); var.test
prior.down <- lda.fit$prior[1]; prior.up <- lda.fit$prior[2]
test[28:38];Lag2[28:38]
z.down <- test*mean(test.down)/var.test - mean(test.down)^2/(2*var.test) +  log(prior.down); head(z.down)
z.up <- test*mean(test.up)/var.test - mean(test.up)^2/(2*var.test) +  log(prior.up); head(z.up)
head(test.up/var.test); head(mean(test.up)^2); head(2*var.test); head(test); head(test.up)
head(z.down); head(z.up)
z <- cbind(z.down, z.up); head(z)
z[29:30,]; data.frame(lda.pred)[29:30,]; Weekly[29:30,]; test[29:30]; mean(test.up); mean(test.down); var.test; prior.down; prior.up

data.frame(lda.pred)[25:35,]
-4.978*(-0.03568254)/10.22933 - (-0.03568254)^2/(2*10.22933) + log(0.4477157) #down
-4.978*0.2603658/10.22933 - 0.2603658^2/(2*10.22933) + log(0.5522843) #up

-2.292*(-0.03568254)/10.22933 - (-0.03568254)^2/(2*10.22933) + log(0.4477157) #down
-2.292*0.2603658/10.22933 - 0.2603658^2/(2*10.22933) + log(0.5522843) #up
f.down <- prior.down*exp(-(x-mean(test.down)^2)/2*var.test)
f.up <- prior.up*exp(-(x-mean(test.up)^2)/2*var.test)
p.down <- f.down/(f.down + f.up); p.down
p.up <- f.up/(f.down + f.up) ; p.up
subset(data.frame(lda.pred), class=="Down")
predict(lda.fit)$x[1]
lda.fit$scaling*Weekly[train,]

# i)
# logistic regression model
summary(train)
library(MASS)
library(ISLR)
names(Weekly)
summary(Weekly)
str(Weekly)
?Weekly
attach(Weekly)
train = Year<2009
pairs(Weekly, col=Direction, cex.labels=2)
?pairs
glm.fit = glm(Direction~Lag1*Lag2*Lag3, data=Weekly, subset=train, family=binomial)
summary(glm.fit)
glm.prob = predict(glm.fit, newdata=Weekly[!train,], type="response")
glm.pred = rep("Down", length(glm.prob))
glm.pred[glm.prob > 0.5] = "Up"
table(glm.pred, Direction[!train])
mean(glm.pred==Direction[!train])

# linear discriminate analysis
lda.fit = lda(Direction~Lag1*Lag2*Lag3, data=Weekly, subset=train)
lda.pred = predict(lda.fit, newdata=Weekly[!train,])
(lda.fit)
data.frame(lda.pred)[1:10,]
table(lda.pred$class, Direction[!train])
mean(lda.pred$class==Direction[!train])

# qudratic discriminate analysis
qda.fit = qda(Direction~poly(Lag1, degree=2)+poly(Lag2, degree=2), data=Weekly, subset=train)
qda.pred = predict(qda.fit, Weekly[!train,])
table(qda.pred$class, Direction[!train])
mean(qda.pred$class==Direction[!train])

# KNN
library(class)
set.seed(1)
stdzd.Weekly = cbind(Weekly[,c(1,9)],scale(Weekly[,-c(1,9)]))
knn.pred = knn(as.matrix(stdzd.Weekly$Lag2[train]), as.matrix(stdzd.Weekly$Lag2[!train]), stdzd.Weekly$Direction[train],k=20)
table(knn.pred, stdzd.Weekly$Direction[!train])
mean(knn.pred==stdzd.Weekly$Direction[!train])

# Question 11
# a)
pairs(Auto, col=cylinders, cex.labels=1.5)
mpg01 <- ifelse(mpg>median(mpg),1,0)
Auto.edit <- data.frame(Auto, mpg01)
summary(Auto.edit)
unique(year)
attach(Auto.edit)

# b)
pairs(Auto.edit, col=year, cex.labels=1.3)
cor(Auto.edit[,-9]) # the correlation matrix shows the most relevant items for predicting mpg01
boxplot(Auto.edit[,-5])

# c)
train <- as.numeric(row.names(Auto.edit))<300
length(mpg01[train])
length(mpg01[!train])

# d) LDA
lda.fit <- lda(mpg01~cylinders+weight+displacement+horsepower, data=Auto.edit, subset=train)
lda.pred <- predict(lda.fit, newdata=Auto.edit[!train,])
data.frame(lda.pred)[1:10,]
table(lda.pred$class, Auto.edit$mpg01[!train])
mean(lda.pred$class!=Auto.edit$mpg01[!train])

# e) QDA
qda.fit <- qda(mpg01~cylinders+weight+displacement+horsepower, data=Auto.edit, subset=train)
qda.pred <- predict(qda.fit, newdata=Auto.edit[!train,])
table(qda.pred$class, Auto.edit$mpg01[!train])
mean(qda.pred$class!=Auto.edit$mpg01[!train])

# f) GLM
glm.fit <- glm(mpg01~cylinders+weight+displacement+horsepower, data=Auto.edit, subset=train, family=binomial)
glm.prob <- predict(glm.fit, newdata=Auto.edit[!train,], type="response")
glm.pred <- ifelse(glm.prob>0.5, "1", "0")
table(glm.pred, mpg01[!train])
mean(glm.pred!=mpg01[!train])

# g) KNN
library(class)
set.seed(1)
names(Auto.edit)
knn.pred <- knn(Auto.edit[,c(2:5)][train,], Auto.edit[,c(2:5)][!train,], mpg01[train], k=20)
table(knn.pred, mpg01[!train])
mean(knn.pred!=mpg01[!train])

# Question 12

# a)
Power <- function() {
        2^3
}
print(Power())

# b)
Power2 <- function(x,a) {
        x^a
}
Power2(3,8)

# c)
Power2(10,3); Power2(8,17); Power2(131,3)

# d)
Power3 <- function(x,a) {
        result <- x^a
        return(result)
}

# e)
plot(c(1:10), Power3(1:10,2), log="xy", main="Question 12", sub="(e)", xlab="log x", ylab="log y")

# f)
PlotPower <- function(x,a) {
        plot(x, x^a, log="xy", main="plot of x against y", xlab="log x", ylab="log y")
}
PlotPower(1:10,3)

# Question 13
?Boston
names(Boston)
summary(Boston)
attach(Boston.edit)
Boston.edit <- Boston
str(Boston)
cor(Boston)
pairs(Boston, cex.labels=1.5)
plot(crim)
?Boston
View(Boston)

train <- as.numeric(row.names(Boston)) <= rep(nrow(Boston)/2, nrow(Boston))
train
crim01 <- ifelse(crim>median(crim), 1, 0)
Boston.edit <- cbind(Boston, crim01)
head(Boston.edit)

# GLM
glm.fit <- glm(crim01~.-crim-crim01, data=Boston.edit, family=binomial, subset=train)
glm.prob <- predict(glm.fit, newdata=Boston.edit[!train,], type="response")
glm.pred <- ifelse(glm.prob>0.5, 1, 0)
table(glm.pred, crim01[!train])
mean(glm.pred!=crim01[!train])

# LDA
lda.fit <- lda(crim01~.-crim-crim01, data=Boston.edit, subset=train)
lda.pred <- predict(lda.fit, newdata=Boston.edit[!train,])
table(lda.pred$class, crim01[!train])
mean(lda.pred$class!=crim01[!train])

# KNN
library(class)
set.seed(1)
attach(Boston.edit)
names(Boston.edit)
knn.pred <- knn(Boston.edit[,-c(1,15)][train,], Boston.edit[,-c(1,15)][!train,], crim01[train], k=10)
table(knn.pred, crim01[!train])
mean(knn.pred!=crim01[!train])

# Web Question 5.R Review Questions
library(ISLR)
names(Portfolio)
str(Portfolio)
summary(Portfolio)
plot(Portfolio)
?Portfolio
load("5.R.RData")
attach(Xy)
par(mfrow=c(1,1))
plot(X1);plot(X2);plot(y)
lm.fit <- lm(y~., data=Xy)
summary(lm.fit)
matplot(Xy, type="l")
?plot
matplot(X1)
plot(X2)
coef(lm.fit)
boot.fn <- function(data, index){
    return(coef(lm(y~., data=Xy, subset=index)))
}
set.seed(1)
boot.fn(Xy, sample(nrow(Xy),nrow(Xy),replace=T))

library("boot")
boot.out <- boot(Xy, boot.fn, 1000)
plot(boot.out)

boot.fn <- function(data, index){
        x <- sample(seq(1, 901, 100), replace=T)
        index = as.vector(mapply(seq, from = x, to = x+99))
        return(coef(lm(y~., data=Xy, subset=index)))
}

boot(Xy, boot.fn, 1000)

# Exercise chapter 5
# Question 1 
# g)
tail(sapply(c(1:100000), function(x) 1-(1-(1/x))^x))

# h)
store <- rep(NA,10000)
for(i in 1:10000){
        store[i] <- sum(sample(1:100, rep=TRUE) ==4)>0
}
mean(store)

## Chapter 5 5.4 Exercise
 # Question 5
 # a)
?Default
glm.fit <- glm(default~income+balance, data=Default, family=binomial)
glm.fit
summary(glm.fit)

 # b) c)
set.seed(1)
train <- sample(1:nrow(Default), nrow(Default)/2)
test <- (-train)
glm.fit <- glm(default~income+balance, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit, newdata=Default[test,], type="response") 
glm.pred <- ifelse(glm.probs>0.5, "Yes", "No")
table(glm.pred, Default[test,]$default)
mean(glm.pred!=Default[test,]$default)
# depending on the subseting, the result of test error rate varies 

 # d)
set.seed(1)
train <- sample(1:nrow(Default), nrow(Default)/2)
test <- (-train)
glm.fit <- glm(default~income+balance+student, data=Default, subset=train, family=binomial)
glm.probs <- predict(glm.fit, newdata=Default[test,], type="response") 
glm.pred <- ifelse(glm.probs>0.5, "Yes", "No")
table(glm.pred, Default[test,]$default)
mean(glm.pred!=Default[test,]$default)
# it increases the test error rate

# Question 6 
# a)
set.seed(1)
summary(glm.fit) # stderr = 7.563e-06 for income and 3.467e-04 for balance

# b)
boot.fn <- function(data, index){
        glm.fit <- glm(default~income+balance, data=data, subset=index, family=binomial)
        glm.fit$coefficients
}
boot.fn(Default, train)
summary(glm.fit)

# c)
boot.out <- boot(Default, boot.fn, R=50)
boot.out
plot(boot.out)
View(boot.out)

# d)
 # The values obtained by glm() are smaller than those by bootstrap. In general they are very similar though.

# Question 7
# a)
set.seed(1)
glm.fit <- glm(Direction~Lag1+Lag2, data=Weekly, family=binomial)
summary(glm.fit)

# b)
glm.fit_1 <- glm(Direction~Lag1+Lag2, data=Weekly[-1,], family=binomial)
summary(glm.fit_1)

# c)
glm.probs <- predict(glm.fit_1, newdata=Weekly[1,], type="response")
glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
glm.pred # it did correctly predict the direction (predicted "Down" Vs. observed "Up")

# d)
n <- nrow(Weekly)
count <- rep(0,n)
for (i in 1:n){
        glm.fit_1 <- glm(Direction~Lag1+Lag2, data=Weekly[-i,], family=binomial)
        glm.probs <- predict(glm.fit_1, newdata=Weekly[i,], type="response")
        glm.pred <- ifelse(glm.probs>0.5, "Up", "Down")
        count[i] <- ifelse(glm.pred!=Weekly[i,]$Direction, 1, 0)
}

# e)
sum(count); mean(count)

# Question 8
# a)
set.seed(1)
x <- rnorm(100)
y <- x - 2*x^2 + rnorm(100) # n = 100, p = 2, y = x - 2*x^2 + error

# b)
plot(x,y) # it is a qudratic function

# c) d)
set.seed(1)
df <- data.frame(x, y)
glm.fit <- glm(y~x, data=df) # i model
cv.glm(df, glm.fit)$delta

glm.fit <- glm(y~poly(x, 2), data=df) # ii model
cv.glm(df, glm.fit)$delta

glm.fit <- glm(y~poly(x, 3), data=df) # iii model
cv.glm(df, glm.fit)$delta

glm.fit <- glm(y~poly(x, 4), data=df) # iv model
cv.glm(df, glm.fit)$delta

# 0.9539049 0.9534453 poly = 4 set.seed(2)
# 0.9539049 0.9534453 poly = 4 set.seed(1)
# same result

# e) the model ii has the smallest cv.error. It is as expected, as the true model is quadratic.

# f) 
summary(glm.fit) # the p-values of x and x^2 are extremly small indicating the highly statistical significance

# Question 9
# a)
set.seed(1)
library(MASS)
?Boston
attach(Boston)
miu <- mean(medv)
miu

# b)
sd(medv)/sqrt(length(medv))

# c)
std.boot <- function(data, index){
        mean(data[index])
}
boot.out <- boot(medv, std.boot, R=1000)
boot.out

# d)
c(miu - 2*0.4119, miu + 2*0.4119)
t.test(medv)
# bootstrap t is broader than the t.test estimate

# e)
median(medv)

# f)
median.boot <- function(data, index) return(median(data[index]))
boot(medv, median.boot, R=1000)

# g) Q9 C5.4 p201
quantile(medv, 0.1)

# h) Q9 C5.4 p201
percentile.boot <- function(data,index) return(quantile(data[index],0.1))
boot(medv, percentile.boot, R=1000)

# Chapter 6

# Conceptual p259

# Question 1
# c) true, true, false, false, false

# Question 2
# a) iii. true
# b) iii. true
# c) ii. true

# Question 3
# increasing s from 0 => increasing number of predictors => more flexible
# a) overfitting => training RSS decreases steadily 
        # iv. 
# b) no overfitting problem for test RSS => decreases in the beginning and then increase again
        # ii.
# c) more flexible models has lower bias and higher variance
        # iii.
# d) opposite of above
        # iv.
# e) reamins constant 
        # v.

# Question 4
# Ridge Regression: increasing lambda from 0 => decreasing number of predictors => less flexibility
# a) training RSS will increase steadily
        # iii.
# b) test RSS no overfitting problem and decrease and then increase 
        # ii.
# c) with model becoming simpler the variance decreases
        # iv.
# d) opposite of c)
        # iii.
# e) irreducible errors remains constant
        # v.

# Question 6

# b)
y <- 2
lbd <- 2
beta <- seq(-3, 3, 0.01)
func.lasso <- (y-beta)^2 + lbd*abs(beta)
plot(beta, func.lasso, pch=20)

est.beta <- y - lbd/2
est.func.lasso <- (y-est.beta)^2 + lbd*abs(est.beta)
points(est.beta, est.func.lasso, col="red", pch=20)

# Applied p262

# Question 8
# a)
set.seed(1)
X <- rnorm(100)
e <- rnorm(100)
b0 <- 3
b1 <- 2
b2 <- -3
b3 <- 0.3

# b)
Y <- b0 + b1*X + b2*X^2 + b3*X^3 + e
plot(X,Y)

# c)
library(leaps)
rnd.data <- data.frame(cbind(X,Y)); (rnd.data)
fit.bst <- regsubsets(Y ~ X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10), data=rnd.data, nvmax=10)

plot(fit.bst, scale="Cp") # b0 + X + X^2 + X^3 + X^5
plot(fit.bst, scale="bic") # b0 + X^2 + X^3
plot(fit.bst, scale = "adjr2") # b0 + X + X^2 + X3 + X^5
names(fit.bst)

par(mfrow=c(1,2))

fit.bst.sum <- summary(fit.bst)
cp.min.pt <- which.min(fit.bst.sum$cp)
cp.min.vl <- fit.bst.sum$cp[cp.min.pt]
plot(fit.bst.sum$cp) + points(cp.min.pt, cp.min.vl, col="red", pch=20)

bic.min.pt <- which.min(fit.bst.sum$bic)
bic.min.vl <- fit.bst.sum$bic[bic.min.pt]
plot(fit.bst.sum$bic) + points(bic.min.pt, bic.min.vl, col="red", pch=20)

ar2.max.pt <- which.max(fit.bst.sum$adjr2)
ar2.max.vl <- fit.bst.sum$adjr2[ar2.max.pt]
plot(fit.bst.sum$adjr2) + points(ar2.max.pt, ar2.max.vl, col="red", pch=20)

# d) using forward and backward selection and repeat c)
# forward selection
fit.fwd <- regsubsets(Y ~ X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10), data=rnd.data, nvmax=10, method="forward")

plot(fit.fwd, scale="Cp") # b0 + X + X^2 + X^7
plot(fit.fwd, scale="bic") # same
plot(fit.fwd, scale="adjr2") # same

fit.fwd.sum <- summary(fit.fwd)
cp.min.pt <- which.min(fit.fwd.sum$cp)
cp.min.vl <- fit.fwd.sum$cp[cp.mint.pt]
plot(fit.fwd.sum$cp) + points(cp.min.pt, cp.min.vl, col="red", pch=20)

fit.fwd.sum <- summary(fit.fwd)
bic.min.pt <- which.min(fit.fwd.sum$bic)
bic.min.vl <- fit.fwd.sum$bic[bic.mint.pt]
plot(fit.fwd.sum$bic) + points(bic.min.pt, bic.min.vl, col="red", pch=20)

fit.fwd.sum <- summary(fit.fwd)
adjr2.max.pt <- which.max(fit.fwd.sum$adjr2)
adjr2.max.vl <- fit.fwd.sum$adjr2[adjr2.max.pt]
plot(fit.fwd.sum$adjr2) + points(adjr2.max.pt, adjr2.max.vl, col="red", pch=20)

# forward stepwise selection choses the model b0 + X + X^2 + X^7 using the three criterion cp, bic and adjr2

# backward selection
fit.bwd <- regsubsets(Y ~ X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10), data=rnd.data, nvmax=10, method="backward")

plot(fit.bwd, scale="Cp") # b0 + X + X^2 + X^9
plot(fit.bwd, scale="bic") # same
plot(fit.bwd, scale="adjr2") # same

fit.bwd.sum <- summary(fit.bwd)
cp.min.pt <- which.min(fit.bwd.sum$cp)
cp.min.vl <- fit.bwd.sum$cp[cp.min.pt]
plot(fit.bwd.sum$cp) + points(cp.min.pt, cp.min.vl, col="red", pch=20)

fit.bwd.sum <- summary(fit.bwd)
bic.min.pt <- which.min(fit.bwd.sum$bic)
bic.min.vl <- fit.bwd.sum$bic[bic.min.pt]
plot(fit.bwd.sum$bic) + points(bic.min.pt, bic.min.vl, col="red", pch=20)

fit.bwd.sum <- summary(fit.bwd)
adjr2.max.pt <- which.max(fit.bwd.sum$adjr2)
adjr2.max.vl <- fit.bwd.sum$adjr2[adjr2.max.pt]
plot(fit.bwd.sum$adjr2) + points(adjr2.max.pt, adjr2.max.vl, col="red", pch=20)

# backward stepwise selection selects the model b0 + X + X^2 + X^9 using cp, bic and adjr2
# forward and backward selection select different models

# e) Lasso 
library(glmnet)
X <- model.matrix(Y ~ X+I(X^2)+I(X^3)+I(X^4)+I(X^5)+I(X^6)+I(X^7)+I(X^8)+I(X^9)+I(X^10), data=rnd.data)
Y <- Y

fit.lasso <- glmnet(X, Y, alpha=1)
plot(fit.lasso, xvar="lambda", labe=TRUE)
cv.lasso <- cv.glmnet(X, Y, alpha=1)
cv.lasso$lambda.min
names(cv.lasso); cv.lasso
plot(cv.lasso)
coef(cv.lasso)
# lasso and best subset selection selects the same model

# f) Lasso and best subset for new model Y = b0 + b7X^7 + e

# Question 9 p263
# a) split data into training and test sets

library(ISLR)
set.seed(11)
sum(is.na(College))

n= nrow(College) / 2 ; train.size
train = sample(1:dim(College)[1], n)
test = -train

# b) fit linear regression with least square
lm.fit = lm(Apps~., data=College, subset=train)
lm.pred = predict(lm.fit, College[test,])
mean((College$Apps[test] - lm.pred)^2)
# Test RSS is 1538442

# c) ridge regression, choose lambda by cv and report test error
X <- model.matrix(Apps~., data=College)
Y <- College$Apps
length(Y); nrow(X)
#ridge.fit <- glmnet(X[train,], Y[train], alpha=0)
grid = 10 ^ seq(4, -2, length=100)
cv.ridge <- cv.glmnet(X[train,], Y[train], alpha=0, lambda=grid, thresh=1e-12)
plot(cv.ridge)
coef(cv.ridge)
lambda.best <- cv.ridge$lambda.min

ridge.pred <- predict(cv.ridge, newx=X[test,], s=lambda.best)
mean((ridge.pred - Y[test])^2)
# THe RSS is 1608859

# d) fit a lasso on training data, choose lambda by cross-validation and report error rate
lasso.tr <- glmnet(X[train,], Y[train], alpha=1)
lasso.pred <- predict(lasso.tr, X[test,])
rsme <- sqrt(apply((lasso.pred-Y[test])^2, 2, mean))
plot(log(lasso.tr$lambda), rsme, type="b", xlab="Log(Lmbda)")
lam.best <- lasso.tr$lambda[order(rsme)[1]] ; lam.best
coef(lasso.tr, s=lam.best)

# Question 10 p265
# (a) generate p=20 features, n=1000 observations, 
set.seed(1)
p <- 20
n <- 1000
x <- matrix(data=rnorm(n*p), nrow=n, ncol=p)
B <- rnorm(p)
B[3] <- 0
B[4] <- 0
B[9] <- 0
B[10] <- 0
B[19] <- 0
eps <- rnorm(p)
y <- x%*%B + eps
# b)
train <- sample(seq(1000), 100, replace=FALSE)

# c)
fit.best <- regsubsets(y~., data=data.frame(x=x[train,], y=y[train]), nvmax=p)
x.train <- model.matrix(y~., data=data.frame(x=x[train,], y=y[train]))
val.errors <- rep(NA, p)
for (i in 1:p){
        coefi <- coef(fit.best, id=i)
        pred <- x.train[,names(coefi)]%*%coefi
        val.errors[i] <- mean((y[train]-pred)^2)
}
plot(val.errors, ylab="Training MSE", pch=19, type="b")

# d)
x.test <- model.matrix(y~., data=data.frame(x=x[-train,], y=y[-train]))
val.errors <- rep(NA, p)
for (i in 1:p){
        coefi <- coef(fit.best, id=i)
        pred <- x.test[,names(coefi)]%*%coefi
        val.errors[i] <- mean((y[-train]-pred)^2)
}
plot(val.errors, ylab="Testing MSE", pch=19, type="b")

# (e)
which.min(val.errors) # 16

# (f)
summary(fit.best)
coef(fit.best, id=16)
# b3 b4 b9 b10 b19 equal to zeor. Except b19 the model selected by best subset method is close to the true one.

# (g)

# Question 11 p264 
# Predict per capita crime rate in Boston data

# (a) using lasso, ridge, best subset, backward/forward selectiono

# best subset
set.seed(1)
library(leaps)
library(MASS)
?Boston
names(Boston)
sum(is.na(Boston))

predict.regsubsets <- function(object, newdata, id,...){
        form <- as.formula(object$call[[2]])
        mat <- model.matrix(form, newdata)
        coefi <- coef(object, id=id)
        mat[,names(coefi)]%*%coefi
}t

folds <- sample(rep(1:10, length=nrow(Boston)))
table(folds)
dim(Boston)
k <- 10
p <- dim(Boston)[2] - 1
cv.errors <- matrix(NA, k, p)

for (i in 1:k){
        fit.best <- regsubsets(crim~., data=Boston[folds!=i,], nvmax=p)
        for (j in 1:p){
                pred <- predict.regsubsets(fit.best, Boston[folds==i,], id=j)
                cv.errors[i,j] <- mean((pred-Boston$crim[folds==i])^2)
        }
        
}
cv.errors
rmse.cv <- sqrt(apply(cv.errors, 2, mean))
plot(rsme.cv, pch=19, type="b")
which.min(rmse.cv)
rmse.cv[which.min(rmse.cv)]
coef(fit.best, id=2)
# best subset chooses b0, rad and lstat with rmse = 3.5

# lasso
x <- model.matrix(crim~.-1, data=Boston)
y <- Boston$crim
cv.lasso <- cv.glmnet(x, y, alpha=1)
plot(cv.lasso)
coef(cv.lasso)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
# lasso chooses b0 and rad with rmse =  7.411171

# ridge
cv.ridge <- cv.glmnet(x, y, alpha=0)
plot(cv.ridge)
coef(cv.ridge)
sqrt(cv.lasso$cvm[cv.lasso$lambda == cv.lasso$lambda.1se])
# ridge regression chooses 13 predictors with rmse = 7.405176

# (b) choose lasso because of interpretability/efficiency or best subset model because of lower rmse



# 7.R Review Questions
# https://lagunita.stanford.edu/courses/HumanitiesSciences/StatLearning/Winter2016/courseware/43d59889973b4b34a7070918f2a7bb3f/10d2c51f157d456bb6c9a66fababbe16/?child=last

load("D:/coursera/Online_course/SL/7.R.RData")
par(mfrow=c(1,1))
plot(x,y)
fit <- lm(y~x)
fit
fit <- lm(y~1+x+I(x^2))
fit
plot(fit)



# 7.9 Exercises p297

# Conceptual

# Question 1
# (a) a1=b0+0 b1=b1 c1=b2 d1=b3
# (b) a1=b0+b4 b1=b1 c1=b2 d1=b3
# (c) 
# (d) 

# Applied

# Question 6 p299
# (a) 
# Perform cross validation to determine the degree of polynomial

library(ISLR)
library(boot)
set.seed(1)
p <- 10
cv.error <- rep(0,10)
for (i in 1:p){
        fit <- glm(wage~poly(age,i), data=Wage)
        cv.error[i] <- cv.glm(Wage,fit,K=10)$delta[2]
}
plot(1:10,cv.error,xlab="poly degree",type="l",pch=20,lwd=2)
lines(1:10,cv.error)
points(which.min(cv.error),cv.error[which.min(cv.error)],col="red",type="o")

# use anova function

fit.1 = lm(wage~poly(age, 1), data=Wage)
fit.2 = lm(wage~poly(age, 2), data=Wage)
fit.3 = lm(wage~poly(age, 3), data=Wage)
fit.4 = lm(wage~poly(age, 4), data=Wage)
fit.5 = lm(wage~poly(age, 5), data=Wage)
fit.6 = lm(wage~poly(age, 6), data=Wage)
fit.7 = lm(wage~poly(age, 7), data=Wage)
fit.8 = lm(wage~poly(age, 8), data=Wage)
fit.9 = lm(wage~poly(age, 9), data=Wage)
fit.10 = lm(wage~poly(age, 10), data=Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6, fit.7, fit.8, fit.9, fit.10)

# plot the polynominal predict function
plot(wage~age,data=Wage,col="darkgrey")
agelim <- range(Wage$age)
age.grid <- seq(agelim[1],agelim[2])
fit <- glm(wage~poly(age,3),data=Wage)
preds <- predict(fit,newdata=list(age=age.grid),se=TRUE)
lines(age.grid,preds$fit,lwd=2,col="darkblue")
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
matlines(age.grid,se.bands,lty=2,col="darkblue")

# (b) fit a step function
cv.error <- rep(0,10)
attach(Wage)
for (i in 2:10){
        Wage$age.cut <- cut(Wage$age,i)
        fit <- glm(wage~age.cut,data=Wage)
        cv.error[i] <- cv.glm(Wage,fit,K=10)$delta[2]
}
plot(2:10,cv.error[-1],xlab="splines",pch=2,type="l",lwd=2)        
# the optimal bins is 8 for step function

# plot the step function with 8 bins
fit <- glm(wage~cut(age,8),data=Wage)
plot(wage~age,data=Wage,col="darkgrey")
preds <- predict(fit,newdata=list(age=age.grid),se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
lines(age.grid,preds$fit,lwd=2,col="darkgreen")
matlines(age.grid,se.bands,lty=2,col="darkgreen")

# Question 7
library(ISLR)
?Wage
names(Wage)
attach(Wage)
summary(Wage)
pairs(Wage)
plot(race,wage)

# try Lasso
library(glmnet)
set.seed(1)

X <- model.matrix(wage~maritl+jobclass+age+race,data=Wage)
Y <- wage

fit.lasso <- glmnet(X, Y, alpha=1)
plot(fit.lasso, xvar="lambda", labe=TRUE)
cv.lasso <- cv.glmnet(X, Y, alpha=1)
cv.lasso$lambda.min
names(cv.lasso)
plot(cv.lasso)
coef(cv.lasso)

fit.lm <- glm(wage~maritl+jobclass+age,data=Wage)
summary(fit.lm)

# QUestion 8
?Auto
names(Auto)
attach(Auto)
pairs(Auto) #shows non-linearity between mpg and (displacement, horsepower,weight)

# Lasso
X <- model.matrix(mpg~.,data=Auto)
Y <- mpg

fit.lasso <- glmnet(X,Y,alpha=1)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso <- cv.glmnet(X,Y,alpha=1)
plot(cv.lasso)
coef(cv.lasso)
# lasso select horsepower, weight, year and origin

# polynomial with predictor horsepower
# selecting polynomial degree using CV
library(boot)
set.seed(1)
p <- 10
cv.error <- rep(0,10)
for (i in 1:p){
        fit <- glm(mpg~poly(displacement,i), data=Auto)
        cv.error[i] <- cv.glm(Auto,fit,K=10)$delta[2]
}
plot(1:10,cv.error,xlab="poly degree",pch=20,lwd=2) + lines(1:10,cv.error) + points(which.min(cv.error),cv.error[which.min(cv.error)],col="red",type="o",pch=20,cex=2)

fit.glm <- glm(mpg~poly(displacement,10),data=Auto)
summary(fit.glm) 
d.lim <- range(displacement)
d.grid <- seq(d.lim[1], d.lim[2])
preds <- predict(fit.glm, newdata=list(displacement=d.grid), se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se, preds$fit+2*preds$se) #preds$se == preds$se.fit
plot(displacement, mpg, col="darkgrey")
lines(d.grid, preds$fit, lwd=2, col="blue")
matlines(d.grid, se.bands, col="blue", lty=2)

# Step function

# Using CV to select cuts for step function

cv.error <- rep(NA,10)
for (i in 2:10){
        Auto$dis.cut <- cut(Auto$displacement,i)
        fit <- glm(mpg~dis.cut,data=Auto)
        cv.error[i] <- cv.glm(Auto,fit,K=10)$delta[2]
}
which.min(cv.error)
plot(2:10,cv.error[-1],xlab="Number of cuts",pch=2,type="l",lwd=2,lty="dashed")
points(which.min(cv.error),cv.error[which.min(cv.error)],col="red",type="o",cex=2,pch=20)
# 9 cuts/bins are selected 

# plot the step function with 9 cuts/bins
fit <- glm(mpg~cut(displacement,9),data=Auto)
plot(mpg~displacement,data=Auto,col="darkgrey")
preds <- predict(fit,newdata=list(displacement=d.grid),se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
lines(d.grid,preds$fit,lwd=2,col="darkgreen")
matlines(d.grid,se.bands,lty=2,col="darkgreen")

# Splines

# natural splines
cv.error <- rep(NA,10)
for (i in 1:10){
        fit <- glm(mpg~ns(displacement,df=i),data=Auto)
        cv.error[i] <- cv.glm(Auto,fit,K=10)$delta[2]
}
which.min(cv.error) # df = 9
# plot the natural splines
fit <- glm(mpg~ns(displacement,df=9),data=Auto)
plot(mpg~displacement,data=Auto,col="darkgrey")
preds <- predict(fit,newdata=list(displacement=d.grid),se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
lines(d.grid,preds$fit,lwd=2,col="purple")
matlines(d.grid,se.bands,lty=2,col="purple")

# smoothing splines
library(splines)
fit2 <- smooth.spline(displacement,mpg,cv=TRUE)
fit # effective degrees of freedom = 20.0332
plot(displacement,mpg,col="darkgrey")
lines(fit,col="purple",lwd=2)
# plot the smoothing splines comparing to natural splines
plot(mpg~displacement,data=Auto,col="darkgrey")
lines(fit2,col="purple",lwd=2)
lines(d.grid,preds$fit,lwd=2,col="red")
legend('topright', legend=c("smoothing splines", "natural splines"),
       col=c("purple", "red"), lty=1:2, cex=0.8)

# GAMs
library(gam)
fit1 <- gam(mpg~s(displacement,df=4),data=Auto)
fit2 <- gam(mpg~s(displacement,df=4)+s(horsepower,df=4),data=Auto)
fit3 <- gam(mpg~s(displacement,df=4)+s(horsepower,df=4)+s(weight,df=4),data=Auto)
fit4 <- gam(mpg~s(displacement,df=4)+s(horsepower,df=4)+s(weight,df=4)+s(year,df=4),data=Auto)

anova(fit1,fit2,fit3,fit4,test='Chisq')
summary(fit4)

# Question 9
set.seed(1)
library(MASS)
attach(Boston)
names(Boston)
# nox~dis

# (a)
fit <- glm(nox~poly(dis,3),data=Auto)
par = (mfrow=c(1,1))
dis.lim <- range(dis)
dis.grid <- seq(dis.lim[1],dis.lim[2],0.1)
preds <- predict(fit,newdata=list(dis=dis.grid),se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se.fit,preds$fit+2*preds$se.fit)
plot(dis,nox,col="darkgrey")
lines(dis.grid,preds$fit,col="red",lwd=2)
matlines(dis.grid,se.bands,lty=2,col="red")

# (b)
all.rss <- rep(NA,10)
for (i in 1:10){
        fit <- lm(nox~poly(dis,i),data=Boston)
        all.rss[i] <- sum(fit$residuals^2)
}
all.rss; which.min(all.rss); plot(1:10,all.rss,type="l") + points(which.min(all.rss),all.rss[which.min(all.rss)],col="red",type='o',pch=20,cex=2)

# (c)
cv.error <- rep(NA,10)
for (i in 1:10){
        fit <- glm(nox~poly(dis,i),data=Boston)
        cv.error[i] <- cv.glm(Boston,fit,K=10)$delta[2]
}
cv.error; which.min(cv.error)
plot(1:10,cv.error,xlab="polynomial degree",lwd=2,type="l")
points(which.min(cv.error),cv.error[which.min(cv.error)],col="red",pch=20,type="o",cex=2)
View(fit)
