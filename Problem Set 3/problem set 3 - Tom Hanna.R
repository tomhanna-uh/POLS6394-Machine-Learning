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

library(MASS)
Boston <- Boston

