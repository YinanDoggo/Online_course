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
