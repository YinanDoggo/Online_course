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
