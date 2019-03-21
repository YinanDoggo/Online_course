x <- 1935
exp(-10.6413+0.0055*x)/(1+exp(-10.6413+0.0055*x))
(log(0.5/(1-0.5))+10.6513)/0.0055

x1 <- 40 ; x2 <- 3.5; b0 <- -6; b1 <- 0.05; b2 <- 1; p <- 0.5
exp(b0+b1*x1+b2*x2)/(1+exp(b0+b1*x1+b2*x2))
(log(0.5/(1-0.5))-b0-b2*x2)/b1

log(0.05/(1-0.05))

library(ISLR)
?Smarket
head(Smarket)
pairs(Smarket, col=Smarket$Direction)
attach(Smarket)
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
glm.probs = predict(glm.fit, type="response")        
glm.probs[1:5]
glm.pred = ifelse(glm.probs>0.5, "Up","Down")
table(glm.pred, Direction)
mean(glm.pred == Direction)

train = Year<2005
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag4+Lag5+Volume, data=Smarket, family=binomial,subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Up","Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)

glm.fit = glm(Direction~Lag1+Lag2, data=Smarket, family=binomial,subset=train)
glm.probs = predict(glm.fit, newdata=Smarket[!train,], type="response")
glm.pred = ifelse(glm.probs>0.5, "Up","Down")
Direction.2005 = Smarket$Direction[!train]
table(glm.pred, Direction.2005)
mean(glm.pred == Direction.2005)
summary(glm.fit)

#linear disciminatn analysis
library(MASS)
library(ISLR)

lda.fit = lda(Direction~Lag1+Lag2, data=Smarket, subset=Year<2005)
lda.fit
plot(lda.fit)
Smarket.2005 = subset(Smarket, Year==2005)
lda.pred = predict(lda.fit, Smarket.2005)
lda.pred[1:5,]
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class, Smarket.2005$Direction)
mean(lda.pred$class == Smarket.2005$Direction)

head(lda.pred$posterior)
sum(lda.pred$posterior[,1]>=0.5)
sum(lda.pred$posterior[,1]<0.5)
sum(lda.pred$class == "Down")
sum(lda.pred$class == "Up")

lda.pred$posterior[1:20,1]
lda.pred$class[1:20]

sum(lda.pred$posterior[,1]>0.9)
max(lda.pred$posterior[,1])

#K-Nearest Neighbors
library(class)
?knn
Xlag = cbind(Lag1,Lag2)
head(Xlag)
train=Year<2005
knn.pred = knn(Xlag[train,], Xlag[!train,], Direction[train], k=1)
table(knn.pred, Direction[!train])
train.X = cbind(Lag1,Lag2)[train,]
test.X = cbind(Lag1,Lag2)[!train,]
train.Direction = Direction[train]
Direction.2005 = Direction[!train]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)

#Caravan Insurance Data
library(ISLR)
summary(Caravan)
attach(Caravan)
summary(Purchase)
standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(standardized.X[,1])

test = 1:1000
train.X = standardized.X[-test,]
test.X = standardized.X[test,]
train.Y = Purchase[-test]
test.Y = Purchase[test]
set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")
result = table(knn.pred, test.Y)
result[2,2]/(result[2,1]+result[2,2])

glm.fits = glm(Purchase~., data=Caravan, family = binomial, subset = -test)
glm.probs = predict(glm.fits, Caravan[test,], type="response")
glm.pred = rep("No",1000)
glm.pred[glm.probs>0.25] = "Yes"
table(glm.pred, test.Y)
summary(test.Y)

table(knn.pred,test.Y)
head(test.Y)
head(knn.pred)