rm(list = ls())

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 2")

# logit2prob <- function(logit){
#     odds <- exp(logit)
#     prob <- odds / (1 + odds)
#     return(prob)
# }



# 5. a) QDA will perform better on the training set, but will overfit the test set.  LDA will perform better on the test set.
# b) QDA, QDA
# c) Improve because with higher sample size QDA can take advantage of the separate covariance matrices to fit a better model. 
# d) False. If the boundary is linear, QDA can only improve on LDA by overfitting. 

#6

#a

B_0 <- -6
B_1 <- 0.05
B_2 <- 1
X_1 <- 40
X_2 <- 3.5

prob <- exp(B_0 + (B_1*X_1) + (B_2*X_2))/(1 + exp(B_0 + (B_1*X_1) + (B_2*X_2)))
prob

#a)	[1] 0.377540

# b)	.5 = exp(-6 + 0.05*X_1 + 3.5*1)/(1 + exp(-6 + .05*X_1 + 3.5*1)
#                                     .5*(1 + exp( .05*X_1 – 2.5)=  exp(.05*X_1 – 2.5)
#                                         .5 + .5*exp exp(.05*X_1 – 2.5))=  exp(.05*X_1 – 2.5)=
#                                         .5 = .5( exp(.05*X_1 – 2.5))
#                                     1 = exp(.05*X_1 – 2.5)=
#                                         log(1) = .05X_1 – 2.5
#                                     2.5 = .05 X_1
#                                     X_1 = 50
#                                     50 hours
#                                     

#7 

# pi-yes = .8
# pi-no = .2
# mu-yes = 10
# mu-no = 0
# variance = 36
# P(Yes|4)  is  0.04033
# P(No|4) is 0.05324
# P(A) is .8 P(B) is .2
# P(A|4) = .8*.04033/(.8*.04033 + .2*0.05324)
# Probability of a dividend is [1] 0.7518643 or 75.2%


#10

#A - Volume increases with year

library(ISLR)

weekly <- as.data.frame(Weekly)

head(weekly)

#create factor with 0,1 values for Direction
#Down = 0, Up = 1

weekly$direction.f <- factor(weekly$Direction,levels = c("Down","Up"), labels = c(0,1))

#drop non-numeric Direction variable

weekly <- weekly[ -c(9)]

str(weekly)

weekly$direction.f <- as.numeric(weekly$direction.f)

str(weekly)

pairs(weekly)

summary(weekly)

cov(weekly)

cor(weekly)

#a - volume increases with year

Weekly <- as.data.frame(Weekly)

model1 <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Weekly,
              family = binomial(link = "logit"))
summary(model1)

#(b) Lag2 is significant at the .05 level

p1 = predict(model1, type = "response")
p2 = rep("Down", length(p1))
p2[p1 > 0.5] = "Up"      
table(p2, Weekly$Direction)

#c - The percentage of correct predictions (Up/Up and Down/Down - the top left and bottom right cells) is 

(54+557)/(54 + 48 + 430 + 557)*100

#Out of the 605 weeks the market was up, it predicted correctly 557 times for correct prediction

(557/605)*100

#of the 484 down weeks, it predicted correctly 54 times for a percentage correct of only

(54/484)*100

#the model overpredicts Up significantly overall.

newdata <- subset(mydata, age >= 20 | age < 10,
                  select=c(ID, Weight))

train1 <- subset(Weekly, Year < 2009)
test1 <- subset(Weekly, Year > 2008)
model2 <- glm(Direction ~ Lag2, data = train1, family = binomial(link = "logit"))

p3 = predict(model2, test1, type = "response")
p4 = rep("Down", length(p3))
p4[p3 > 0.5] = "Up"      
table(p4, test1$Direction)

#Correct predictions in test data is 
(9+56)/(9 + 5 + 34 + 56)


#e

library(MASS)
lda.model1 <- lda(Direction ~ Lag2, data = train1)
lda.predict <- predict(lda.model1,test1)
table(lda.predict$class,test1$Direction)

#Correct prediction for LDA is
(9+56)/(9+5+34+56)

#f
qda.model1 <- qda(Direction ~ Lag2, data = train1)
qda.cl <- predict(qda.model1,test1)$class
table(qda.cl,test1$Direction)

#Correct predictions for QDA is
(61)/(43 + 61)

#g
library(class)
train.X <- as.matrix(train1$Lag2)
test.X <- as.matrix(test1$Lag2)
set.seed(1)
knn.p <- knn(train.X, test.X, train1$Direction, k = 1)
table(knn.p,test1$Direction)

#Correct predictions for KNN with k = 1 is 
(21+31)/(21+30+31+22)

#h LDA and logit produce the best error rate with 0.625 correct predictions.

#i

qda.model3 <- qda(Direction ~ Lag2^2, data = train1)
qda.c2 <- predict(qda.model3,test1)$class
table(qda.c2,test1$Direction)

qda.model4 <- qda(Direction ~ Lag2*Year, data = train1)
qda.c3 <- predict(qda.model4,test1)$class
table(qda.c3,test1$Direction)

#correct prediction for QDA model 4 is
(24+23)/(24+38+19+23)

#The model did better always predicting "Up"

train.X2 <- as.matrix(train1$Lag2)
test.X2 <- as.matrix(test1$Lag2)
set.seed(1)
knn.p2 <- knn(train.X2, test.X2, train1$Direction, k = 20)
table(knn.p2,test1$Direction)

#Correct prediction for KNN with k = 20 is
(21+40)/(21+21+22+40)

#which is an improvement over correct prediction of 0.5 for KNN with k = 1.

##Chapter 5

rm(list = ls())

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 2")

#3
#a  The data is split into k sets. Starting with the first set, the set is used as a validation set
# with the other k-1 sets used as a training set. The Mean Squared Error is evaluated, then the process
#is repeated for each of the k sets. 

#b 
#i Like LOOCV, k-fold has the advantage of always returning the same error rate, unlike the validation
#set approach where randomness can cause variation in the error rate. 
#ii Since LOOCV is a special case of k-fold validation where k = n, LOOCV requires more computation. 

set.seed(1)
x <- rnorm(100)
y = x - 2 * x^2 + rnorm(100)

#n = 100, p = 2
#Y = X - 2(x^2) + e

plot(x,y)

#The relationship is curvilinear and nonmonotonic, with the value of Y first rising, then 
#falling as X increases.

library(boot)
MyData <- data.frame(x, y)
set.seed(1)
# i.
fit1 <- glm(y ~ x)
cv.glm(MyData, fit1)$delta

#ii

fit2 <- glm(y ~ poly(x, 2))
cv.glm(MyData, fit2)$delta

#iii
fit3 <- glm(y ~ (poly(x,3)))
cv.glm(MyData,fit3)$delta

#iv
fit4 <- glm(y ~ (poly(x,4)))
cv.glm(MyData,fit4)$delta

#d

set.seed(95)
MyData <- data.frame(x, y)
set.seed(1)
# i.
fit1 <- glm(y ~ x)
cv.glm(MyData, fit1)$delta

#ii

fit2 <- glm(y ~ poly(x, 2))
cv.glm(MyData, fit2)$delta

#iii
fit3 <- glm(y ~ (poly(x,3)))
cv.glm(MyData,fit3)$delta

#iv
fit4 <- glm(y ~ (poly(x,4)))
cv.glm(MyData,fit4)$delta

#The results are the same because it evaluates k = n test sets that do not change randomly.

#e - Model fit2, the 2nd degree polynomial, had the smallest error. The scatterplot matched a 2nd order
#polynomial, or quadratic, transformation, so this is not surprising.

#f

summary(fit1)
summary(fit2)
summary(fit3)
summary(fit4)

#Adding the squared term improved model fit and caused the x term significance to move above .01. 
#Adding third and fourth degrees to the polynomial increased the AIC, indicating worse fit, 
#and the 3rd and 4th degrees were not significant, though the first two degrees were still 
#significant.