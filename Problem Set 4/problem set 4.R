gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 4")

#4

#a


#b

plot(NA, NA, type = "n", xlim = c(-3, 3), ylim = c(-3, 3), xlab = "X1", ylab = "X2")
lines(x = c(-3, 3), y = c(2, 2))
lines(x = c(-3, 3), y = c(1, 1))
lines(x = c(0, 0), y = c(1, 2))
lines(x = c(1, 1), y = c(-3, 1))
text(x = -1/2, y = -1, labels = -1.8)
text(x = 1.5, y = -1, labels = 0.63)
text(x = 0, y = 2.5, labels = 2.49)
text(x = -1, y = 1.5, labels = -1.06)
text(x = 1, y = 1.5, labels = 0.21)

#![alt text here](sketch.jpg)

#5

p = c(0.1, 0.15, 0.2, 0.2, 0.55, 0.6, 0.6, 0.65, 0.7, 0.75)

sum(p >= 0.5) 

sum(p < 0.5)

#Six probabilities of ten, a majority, are above 0.5. Classification is probability of Red greater
#0.5 or "Red."

mean(p)

#Average is 0.45. So by the average probability method, classification is "not Red," or
#probability of Red less than 0.5

#8

library(ISLR)
attach(Carseats)
set.seed(1)

#a

train = sample(dim(Carseats)[1], dim(Carseats)[1]/2)
Train.Carseats= Carseats[train, ]
Test.Carseats = Carseats[-train, ]

#b

library(tree)
Tree.Carseats = tree(Sales ~ ., data = Train.Carseats)
summary(Tree.Carseats)

plot(Tree.Carseats)
text(Tree.Carseats, pretty = 0)

Carseats.Pred = predict(Tree.Carseats, Test.Carseats)
mean((Test.Carseats$Sales - Carseats.Pred)^2)

#MSE is approximately 4.92

#c

Carseats.cv = cv.tree(Tree.Carseats, FUN = prune.tree)
par(mfrow = c(1, 2))
plot(Carseats.cv$size, Carseats.cv$dev, type = "b")
plot(Carseats.cv$k, Carseats.cv$dev, type = "b")

#Best size is 13

Pruned.Carseats = prune.tree(Tree.Carseats, best = 13)
par(mfrow = c(1, 1))
plot(Pruned.Carseats)
text(Pruned.Carseats, pretty = 0)

pruned.predict = predict(Pruned.Carseats, Test.Carseats)
mean((Test.Carseats$Sales - pruned.predict)^2)

#MSE is approximately 4.97 with pruning

#d

library(randomForest)

Bag.Carseats = randomForest(Sales ~ ., data = Train.Carseats, mtry = 10, ntree = 500, 
                            importance = T)
Predict.Bag = predict(Bag.Carseats, Test.Carseats)
mean((Test.Carseats$Sales - Predict.Bag)^2)

importance(Bag.Carseats)

#MSE is improved to 2.603
#Price, ShelveLoc, and CompPrice are the most important predictors of sales.

#e

RF.Carseats = randomForest(Sales ~ ., data = Train.Carseats, mtry = 5, ntree = 500, 
                           importance = T)
RF.pred = predict(RF.Carseats, Test.Carseats)
mean((Test.Carseats$Sales - RF.pred)^2)

#MSE is 2.721

importance(RF.Carseats)

#The most important predictors of Sales are Price, ShelveLoc, and CompPrice. MSE is 2.721

#Changing m varies the MSE from 4.824 to 2.57. As m increases, MSE decreases.

#10 

#a

library(ISLR)

View(Hitters)

Hitters <- Hitters[-which(is.na(Hitters$Salary)), ]
summary(Hitters$Salary)

Hitters$Salary = log(Hitters$Salary)
summary(Hitters$Salary)

#b

train = 1:200
Train.Hitters = Hitters[train, ]
Test.Hitters = Hitters[-train, ]

#c

library(gbm)

set.seed(75)
pows <- seq(-10, -0.2, by = 0.1)
lambdas <- 10^pows
length.lambdas = length(lambdas)
train.errors = rep(NA, length.lambdas)
test.errors = rep(NA, length.lambdas)
for (i in 1:length.lambdas) {
    Boost.Hitters <- gbm(Salary ~ ., data = Train.Hitters, distribution = "gaussian", 
                        n.trees = 1000, shrinkage = lambdas[i])
    Train.Pred <- predict(Boost.Hitters, Train.Hitters, n.trees = 1000)
    Test.Pred <- predict(Boost.Hitters, Test.Hitters, n.trees = 1000)
    train.errors[i] = mean((Train.Hitters$Salary - Train.Pred)^2)
    test.errors[i] = mean((Test.Hitters$Salary - Test.Pred)^2)
}

plot(lambdas, train.errors, type = "b", xlab = "Shrinkage Values", ylab = "Training Set MSE", 
     col = "red", pch = 20)

#d

plot(lambdas, test.errors, type = "b", xlab = "Shrinkage Values", ylab = "Test Set MSE", 
     col = "purple", pch = 20)

#e

lm.Hitters <- lm(Salary ~ ., data = Train.Hitters)
lm.pred <- predict(lm.Hitters, Test.Hitters)
mean((Test.Hitters$Salary - lm.pred)^2)

library(glmnet)

set.seed(75)
x <- model.matrix(Salary ~ ., data = Train.Hitters)
y <- Train.Hitters$Salary
x.test <- model.matrix(Salary ~ ., data = Test.Hitters)
lasso.fit = glmnet(x, y, alpha = 1)
lasso.pred = predict(lasso.fit, s = 0.01, newx = x.test)
mean((Test.Hitters$Salary - lasso.pred)^2)

#The test MSE of both models - .492 and .471 - are higher than the boosting TEST MSE (see plot) for
#any lambda above ~.001

#f

Best.Boost <- gbm(Salary ~ ., data = Train.Hitters, distribution = "gaussian", 
                 n.trees = 1000, shrinkage = lambdas[which.min(test.errors)])
summary(Best.Boost)

#CAtBat, PutOuts, and CHits are the most influential. 

#g

library(randomForest)

set.seed(75)
RF.hitters = randomForest(Salary ~ ., data = Train.Hitters, ntree = 500, mtry = 19)
rf.pred = predict(RF.hitters, Test.Hitters)
mean((Test.Hitters$Salary - rf.pred)^2)

#MSE with bagging is 0.23

