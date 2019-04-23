library(ISLR)
attach(Wage)

# Polynomials

# single predictor age

fit <- lm(wage~(poly(age, 4)), data=Wage)
summary(fit)

# plot

agelim <- range(age)
age.grid <- seq(agelim[1], agelim[2])
preds <- predict(fit, newdata=list(age=age.grid), se=TRUE)
se.bands <- cbind(preds$fit-2*preds$se, preds$fit+2*preds$se) #preds$se == preds$se.fit
plot(age, wage, col="darkgrey")
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid, se.bands, col="blue", lty=2)

# more direct way in R for polynomials

fita <- lm(wage~age+I(age^2)+I(age^3)+I(age^3)+I(age^4), data=Wage) # this one yields different coefficients 
                                                                    # because it uses different basis functions
summary(fita)

# check wether these two are the same model a.k.a. with same fitted value

plot(fitted(fit),fitted(fita))

# test models using anova()

fita <- lm(wage~education,data=Wage)
fitb <- lm(wage~education+age,data=Wage)
fitc <- lm(wage~education+poly(age,2),data=Wage)
fitd <- lm(wage~education+poly(age,3),data=Wage)
anova(fita,fitb,fitc,fitd)

# Polynomial logistic regression

# whether age>250k

fit <- glm(I(wage>250)~poly(age, 3), data=Wage, family=binomial)
summary(fit)
preds <- predict(fit, newdata=list(age=age.grid), se=T)
se.bands <- preds$fit + cbind(fit=0, lower=-2*preds$se, upper=2*preds$se)
prob.bands <- exp(se.bands)/(1+exp(se.bands))
matplot(age.grid, prob.bands, col="blue", lwd=c(2,1,1), lty=c(1,2,2), type="l", ylim=c(0,0.1))
points(jitter(age),I(wage>250)/10, pch='l', cex=0.5)

# Splines

# Splines are more flexible then polynomial but conceptually are similar

# Cubic splines
library(splines)
fit <- lm(wage~bs(age, knots=c(25,40,60)), data=Wage)
plot(age, wage, col="darkgrey")
lines(age.grid, predict(fit, list(age=age.grid)), col="darkgreen", lwd=2)
abline(v=c(25,40,60), lty=2, col="darkgreen")

# Smoothing splines doesn't require the selection of knots but smoothing parameter, 
# which can be conveniently specified via the effective degrees of freedom

fit <- smooth.spline(age,wage,df=16)
lines(fit,col="red",lwd=2)
fit2 <- smooth.spline(age,wage,df=30) #high df
fit3 <- smooth.spline(age,wage,df=5) #low df
lines(fit2,col="black",lwd=2)
lines(fit3,col="purple",lwd=2)

# Using LOO cross-valadation to select the smoothing parameter
fit <- smooth.spline(age,wage,cv=TRUE)
lines(fit,col="purple",lwd=2)
fit #the effecitve degrees of freedom is about 6,78 in this case. It does not have to be a integer. It's a heuristic 
    #for how rough the function is

# General Additive Models
#------------------------

# So far we focused only on predicting one predictor. The package 'gam' would help us deal with multiple predictors
# and it also has a smart and convenient plot function

install.packages('gam')
library(gam)
gam1 <- gam(wage~s(age,df=4)+s(year,df=4)+education,data=Wage)
par(mfrow=c(1,3))
plot(gam1,se=T)
gam2 <- gam(I(wage>250)~s(age,df=4)+s(year,df=4)+education,data=Wage)
plot(gam2,se=T)

# Use gam to test whether we need a nonlinear term for year

gam2a <- gam(I(wage>250)~s(age,df=4)+year+education,data=Wage)
anova(gam2a,gam2,test="Chisq")

# 'gam' package can be even used to plot model fitted by 'glm' and 'lm'

par(mfrow=c(1,3))
lm1 <- lm(wage~ns(age,df=4)+ns(year,df=4)+education,data=Wage)
plot.Gam(lm1,se=T) #specify plot.Gam to use the plot function of package 'gam'

#further function on 'gam' in help file and a book by rob and trevor on GAM
