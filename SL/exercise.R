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

# i)

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

scale
