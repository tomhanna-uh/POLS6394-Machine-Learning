rm(list = ls())
setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 3")

#ISLR, Chapter 6, #4, #9, #11
#ISLR, Chapter 7, #3, #4, #9, #10

College <- read.csv("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 3/College.csv")
names(College)

set.seed(75)

#9a

train.size = dim(College)[1] / 2
train = sample(1:dim(College)[1], train.size)
test = -train
Train.college = College[train, ]
Test.college = College[test, ]

model1 <- lm(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                 Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
             data = Train.college)
model1.pred <- predict(model1, Test.college)
mean((Test.college[, "Apps"] - model1.pred)^2)

#b - Test RSS is 1672201

library(glmnet)

mat.train <- model.matrix(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                              Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
                          data = Train.college)
mat.test <- model.matrix(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                             Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
                         data = Test.college)
grid <- 10 ^ seq(4, -2, length=100)
model.ridge <- cv.glmnet(mat.train, Train.college[, "Apps"], alpha=0, lambda=grid, thresh=1e-12)
lambda1 <- model.ridge$lambda.min
lambda1

model.ridge.pred <- predict(model.ridge, newx=mat.test, s=lambda1)
mean((Test.college[, "Apps"] - model.ridge.pred)^2)

#c - Test error is 1732682

model.lasso <- cv.glmnet(mat.train, Train.college[, "Apps"], alpha=1, lambda=grid, thresh=1e-12)
lambda2 <- model.lasso$lambda.min
lambda2

model.lasso.pred <- predict(model.lasso, newx=mat.test, s=lambda2)
mean((Test.college[, "Apps"] - model.lasso.pred)^2)

#Test error is 1731804
#Coefficients:

model.lasso <- glmnet(model.matrix(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                                    Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
                                data = College), College[, "Apps"], alpha=1)
predict(model.lasso, s=lambda2, type="coefficients")

library(pls)

model.pcr <- pcr(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                     Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
                 data = Train.college, scale=T, validation="CV")
validationplot(model.pcr, val.type="MSEP")

model.pcr.pred <- predict(model.pcr, Test.college, ncomp = 10)
mean((Test.college[, "Apps"] - model.pcr.pred)^2)

#Test error is 3360184

model.pls <- plsr(Apps ~ Private + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad + Outstate +
                      Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni + Expend + Grad.Rate,
                  data = Train.college, scale=T, validation="CV")
validationplot(model.pls, val.type="MSEP")

model.pls.pred <- predict(model.pls, Test.college, ncomp=10)
mean((Test.college[, "Apps"] - model.pls.pred)^2)

#The test error is 1670383

#g - Ridge, Lasso, and PLS provide accurate predictions. PCR has a higher error rate. 

rm(list = ls())
setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 3")

library(MASS)
Boston <- Boston

set.seed(25)

train.size = dim(Boston)[1] / 2
train = sample(1:dim(Boston)[1], train.size)
test = -train
Train.Boston = Boston[train, ]
Test.Boston = Boston[test, ]

model1 <- lm(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv,
             data = Train.Boston)


model1.pred <- predict(model1, Test.Boston)
mean((Test.Boston[, "crim"] - model1.pred)^2)

#Test RSS is 41.32216

library(glmnet)

mat.train <- model.matrix(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Train.Boston)
mat.test <- model.matrix(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Test.Boston)
grid <- 10 ^ seq(4, -2, length=100)
model.ridge <- cv.glmnet(mat.train, Train.Boston[, "crim"], alpha=0, lambda=grid, thresh=1e-12)
lambda1 <- model.ridge$lambda.min
lambda1

model.ridge.pred <- predict(model.ridge, newx=mat.test, s=lambda1)
mean((Test.Boston[, "crim"] - model.ridge.pred)^2)

#Test error is 41.52214

model.lasso <- cv.glmnet(mat.train, Train.Boston[, "crim"], alpha=1, lambda=grid, thresh=1e-12)
lambda2 <- model.lasso$lambda.min
lambda2

model.lasso.pred <- predict(model.lasso, newx=mat.test, s=lambda2)
mean((Test.Boston[, "crim"] - model.lasso.pred)^2)

#Test error is 41.26353
#Coefficients:

model.lasso <- glmnet(model.matrix(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Boston), Boston[, "crim"], alpha=1)
predict(model.lasso, s=lambda2, type="coefficients")

library(pls)

model.pcr <- pcr(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Train.Boston, scale=T, validation="CV")
validationplot(model.pcr, val.type="MSEP")

model.pcr.pred <- predict(model.pcr, Test.Boston, ncomp = 10)
mean((Test.Boston[, "crim"] - model.pcr.pred)^2)

#Test error is 43.96352

model.pls <- plsr(crim ~ zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data = Train.Boston, scale=T, validation="CV")
validationplot(model.pls, val.type="MSEP")

model.pls.pred <- predict(model.pls, Test.Boston, ncomp=10)
mean((Test.Boston[, "crim"] - model.pls.pred)^2)

#11 -b 

library (leaps)
regfit.full=regsubsets (crim~. ,data = Train.Boston)

reg.summary =summary(regfit.full)
reg.summary
reg.summary$rsq

par(mfrow =c(2,2))
plot(reg.summary$rss ,xlab=" Number of Variables ",ylab=" RSS",
       type="l")
plot(reg.summary$adjr2 ,xlab =" Number of Variables ",
       ylab=" Adjusted RSq",type="l")
plot(reg.summary$bic ,xlab =" Number of Variables ",
     ylab="BIC",type="l")

##BIC is minimized with 2 variables. With 2 features, and "rad" and "lstat" produce the best model.

#C - No. BIC was minimized with 2 features. 

##Chapter 7
#3

x = -2:2
y = 1 + 1*x + -2 * (x-1)^2 * I(x>=1)
plot(x, y)

#4

x=seq(-2,2,by=0.01)
y2 = function(x){
    # b1(X) = I(0 ≤ X ≤ 2) − (X −1)I(1 ≤ X ≤ 2)
    # b2(X) = (X −3)I(3 ≤ X ≤ 4)+I(4 < X ≤ 5)
    1 + (I(x>=0 && x<=2) - ((x-1)*I(x>=1 && x<=2))) + 3*((x-3)*I(x>=3 && x<=4) + I(x>4 && x<=5))
}
yp = sapply(x,FUN=y2)
plot(x,yp,xlim=c(-2,2),ylim=c(0,2.5))
#points(x=x,y=y(x),pch=19,col="red")

#9
A - 
model1 <- lm(nox ~ poly(dis, 3), data = Boston)
model1
B - 

model2 <- lm(nox ~ poly(dis,2), data = Boston)
model3 <- lm(nox ~ poly(dis,3), data = Boston)
model4 <- lm(nox ~ poly(dis,4), data = Boston)
model5 <- lm(nox ~ poly(dis,5), data = Boston)
model6 <- lm(nox ~ poly(dis,6), data = Boston)
model7 <- lm(nox ~ poly(dis,7), data = Boston)
model8 <- lm(nox ~ poly(dis,8), data = Boston)
model9 <- lm(nox ~ poly(dis,9), data = Boston)
model10 <- lm(nox ~ poly(dis,10), data = Boston)

anova(model1)["Residuals", "Sum Sq"]
anova(model2)["Residuals", "Sum Sq"]
anova(model3)["Residuals", "Sum Sq"]
anova(model4)["Residuals", "Sum Sq"]
anova(model5)["Residuals", "Sum Sq"]
anova(model6)["Residuals", "Sum Sq"]
anova(model7)["Residuals", "Sum Sq"]
anova(model8)["Residuals", "Sum Sq"]
anova(model9)["Residuals", "Sum Sq"]
anova(model10)["Residuals", "Sum Sq"]

#c - 
Library(ISLR)
set.seed=(45)
train=sample (405,101)

model1 <- lm(nox ~ poly(dis,1), data = Boston, subset =train)   
model2 <- lm(nox ~ poly(dis,2), data = Boston, subset =train)
model3 <- lm(nox ~ poly(dis,3), data = Boston, subset =train)
model4 <- lm(nox ~ poly(dis,4), data = Boston, subset =train)
model5 <- lm(nox ~ poly(dis,5), data = Boston, subset =train)
model6 <- lm(nox ~ poly(dis,6), data = Boston, subset =train)
model7 <- lm(nox ~ poly(dis,7), data = Boston, subset =train)
model8 <- lm(nox ~ poly(dis,8), data = Boston, subset =train)
model9 <- lm(nox ~ poly(dis,9), data = Boston, subset =train)
model10 <- lm(nox ~ poly(dis,10), data = Boston, subset =train)

mean((Boston$nox - predict(model1,Boston))[-train]^2)
mean((Boston$nox - predict(model2,Boston))[-train]^2)
mean((Boston$nox - predict(model3,Boston))[-train]^2)
mean((Boston$nox - predict(model4,Boston))[-train]^2)
mean((Boston$nox - predict(model5,Boston))[-train]^2)
mean((Boston$nox - predict(model6,Boston))[-train]^2)
mean((Boston$nox - predict(model7,Boston))[-train]^2)
mean((Boston$nox - predict(model8,Boston))[-train]^2)
mean((Boston$nox - predict(model9,Boston))[-train]^2)
mean((Boston$nox - predict(model10,Boston))[-train]^2)

#The validation error is lowest with the 3rd degree polynomial, model3.

#d - 

summary(Train.Boston$dis)

library (splines )

model.spline <- lm(nox ~ bs(dis,df=4,knots =c(2.197,3.272,5.117)), data = Train.Boston)
pred <- predict(model.spline,newdata=Test.Boston,se=T)

par(mfrow =c(2,2))
plot(dis,nox,col="gray")
lines(Test.Boston$dis,pred$fit,lwd=2)
lines(Test.Boston$dis,pred$fit +2* pred$se.fit,lty="dashed")
lines(Test.Boston$dis,pred$fit -2* pred$se.fit,lty="dashed")

#I placed the knots at the quartiles for dis


model.spline6 <- lm(nox ~ bs(dis,df=6), data = Train.Boston)
pred6 <- predict(model.spline6,newdata=Test.Boston,se=T)


plot(dis,nox,col="gray")
lines(Test.Boston$dis,pred6$fit,lwd=2)
lines(Test.Boston$dis,pred6$fit +2* pred$se.fit,lty="dashed")
lines(Test.Boston$dis,pred6$fit -2* pred$se.fit,lty="dashed")

model.spline8 <- lm(nox ~ bs(dis,df=8), data = Train.Boston)
pred8 <- predict(model.spline8,newdata=Test.Boston,se=T)


plot(dis,nox,col="gray")
lines(Test.Boston$dis,pred8$fit,lwd=2)
lines(Test.Boston$dis,pred8$fit +2* pred$se.fit,lty="dashed")
lines(Test.Boston$dis,pred8$fit -2* pred$se.fit,lty="dashed")


model.spline10 <- lm(nox ~ bs(dis,df=10), data = Train.Boston)
pred10 <- predict(model.spline10,newdata=Test.Boston,se=T)


plot(dis,nox,col="gray")
lines(Test.Boston$dis,pred10$fit,lwd=2)
lines(Test.Boston$dis,pred10$fit +2* pred$se.fit,lty="dashed")
lines(Test.Boston$dis,pred10$fit -2* pred$se.fit,lty="dashed")

anova(model.spline)["Residuals", "Sum Sq"]
anova(model.spline6)["Residuals", "Sum Sq"]
anova(model.spline8)["Residuals", "Sum Sq"]
anova(model.spline10)["Residuals", "Sum Sq"]

#The model with 10 degrees of freedom had the lowest RSS. 

#f
library(boot)
all.cv = rep(NA, 16)
for (i in 3:16) {
    spline.modelx = glm(nox ~ bs(dis, df = i), data = Boston)
    all.cv[i] = cv.glm(Boston, spline.modelx, K = 10)$delta[2]
}
par(mfrow = c(1,1))
plot(3:16, all.cv[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")

#Cross validation error is minimized at 12. 


#10A

rm(list = ls())
setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 3")
library(leaps)

College <- read.csv("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 3/College.csv")
names(College)

attach(College)

set.seed(755)
train <- sample(length(Outstate), length(Outstate)/2)
test <- -train
Train.College <- College[train, ]
Test.College <- College[test, ]
reg.fit <- regsubsets(Outstate ~ as.factor(Private)  + Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad +
                          +Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni +
                          Expend + Grad.Rate, data = Train.College, nvmax = 17, method = "forward")
reg.summary <- summary(reg.fit)


par(mfrow = c(1,3))

plot(reg.summary$bic, xlab = "Variables", ylab = "BIC", type = "l")
min.bic <- min(reg.summary$bic)
std.bic <- sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "blue", lty = 2)

plot(reg.summary$adjr2, xlab = "Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 <- max(reg.summary$adjr2)
std.adjr2 <- sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "blue", lty = 2)

plot(reg.summary$cp, xlab = "Variables", ylab = "Cp", type = "l")
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)

#BIC is minimized around 7 and again at 9, CP is minimized around 12, Adjusted R-squared is maxmized around 12. I chose 9
#as a reasonable solution. 

reg.fit <- regsubsets(Outstate ~ as.factor(Private)  + Apps + Accept + Enroll + Top10perc + Top25perc + F.Undergrad + P.Undergrad +
                          +Room.Board + Books + Personal + PhD + Terminal + S.F.Ratio + perc.alumni +
                          Expend + Grad.Rate, data = College, method = "forward")
coefi <- coef(reg.fit, id = 9)
names(coefi)

library(gam)

model.gam <- gam(Outstate ~ as.factor(Private) + s(Room.Board, df = 2) + s(Personal, df = 2) + s(PhD, df = 2) + 
                  s(Terminal, df =2) + s(perc.alumni, df = 2) + s(Expend, df = 5) + s(Grad.Rate, df = 2), 
                 data = Train.College)

par(mfrow = c(3, 3))
plot(model.gam, se = T, col = "blue")

model.gam.pred = predict(model.gam, Test.College)
model.gam.err = mean((Test.College$Outstate - model.gam.pred)^2)
model.gam.err

#Error for the GAM model was 3832618

#D Expend and Terminal are clearly nonlinear. Grad Rate, Personal, perc.alunmi, and PhD show some smaller
#evidence of nonlinearity. Room.Board is the closest to linear. 