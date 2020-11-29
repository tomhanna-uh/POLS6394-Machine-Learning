gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 4")

#Neural Networks

#1


#2

library(ISLR)
library(caret)
library(skimr)

View(Default)

#a

train = 1:8000
Train.Default = Default[train, ]
Test.Default = Default[-train, ]

skim(Train.Default)
skim(Test.Default)

#b

preprocess.missingdata.model <- preProcess(Train.Default, method = 'knnImpute')
preprocess.missingdata.model

library(RANN)

Train.Default <- predict(preprocess.missingdata.model, newdata = Train.Default)

anyNA(Train.Default)


dummies_model <- dummyVars( ~ ., data = Train.Default)

trainData_mat <- predict(dummies_model, newdata = Train.Default)

Train.Data <- data.frame(trainData_mat)
str(Train.Data)

preProcess_range_model <- preProcess(Train.Data, method = "range")
Train.Data <- predict(preProcess_range_model, newdata = Train.Data)
skim(Train.Data)

##Wasted a lot of time getting errors because of the "default" variable. Tried including it in
##one hot encoding, tried converting to a binary numeric factor before preprocessing,
##Finally managed to get preprocessing done and model to converge by doing this
##Better method???

Train.Data <- subset(Train.Data, select = -c(default.Yes,default.No))
Train.Data$default <- Train.Default$default
str(Train.Data$default)

#c

library(e1071)

set.seed(75)

modelnn <- train(default ~ ., data = Train.Data, method='nnet')

prednn1 <- predict(modelnn, Train.Data)
modelnn

plot(modelnn, main = "Model Accuracies with NNet")

##Pre-process Test Data

TestData2 <- predict(preprocess.missingdata.model, Test.Default)

TestData3 <- predict(dummies_model, TestData2)

TestData4 <- predict(preProcess_range_model, TestData3)

TestData4 <- data.frame(TestData4)

Test.Data <- subset(TestData4, select = -c(default.Yes,default.No))
Test.Data$default <- Test.Default$default
str(Test.Data$default)

prednn2 <- predict(modelnn, Test.Data)
head(prednn2)

outcome <- factor(Test.Data$default, levels = c("Yes","No"))
confusionMatrix(reference = outcome, data = prednn2, mode = "everything", positive = 'Yes')   

##Accuracy is high, but sensitivity is low. It missed a lot of positive predictions, getting 24 correct
##out of 67 actual positives, a sensitivity of .3582.

#d

fitControl <- trainControl(
    method = "cv",
    number = 10,
    savePredictions = "final",
    classProbs = T,
    summaryFunction = twoClassSummary
)

set.seed(75)
model_nnet_cv <- train(default ~ ., data = Train.Data, method = "nnet", metric = "ROC",
                       trControl = fitControl, trace = FALSE)

model_nnet_cv

plot(model_nnet_cv, main = "Model ROC with NNet")


#best model is unchanged - size 3 and decay .1

#e

set.seed(75)

model.boost <- train(default ~ ., data = Train.Data, method = "adaboost", tuneLength = 2, 
                     trControl = fitControl)
model.boost

#The best ROC with boosting was .9192, quite a bit less than the best ROC of ~ .96 with NNet

model.rf <- train(default ~ ., data = Train.Data, method = "rf", tunelength = 5, trControl = fitControl)
model.rf

#The best ROC with random forest was 0.8980, considerably less than the best ROC of ~ .96 with NNet.



#Chapter 8

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

