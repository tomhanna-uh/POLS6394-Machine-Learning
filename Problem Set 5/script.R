gc()
rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 5")

##Chapter 9

#3

#a

set.seed(75)
x1 = c(3, 2, 4, 1, 2, 4, 4)
x2 = c(4, 2, 4, 4, 1, 3, 1)
colors = c("red","red","red","red","blue","blue","blue")


data <- as.data.frame(cbind(x1,x2,colors))

plot(data$x1,data$x2, col = colors)

#b 

#β0 + β1X1 + β2X2 = 0 (from 9.1)

#The equation is -.5 + X1 - X2 = 0 


abline(-0.5, 1)

#c

#Classify as Red if -.5 + X1 - X2 < 0 
#Classify as Blue if -.5 + X1 - X2 > 0 

#d

abline(-1, 1, lty = 2)
abline(0, 1, lty = 2)

#e 

plot(data$x1, data$x2, col = colors)
abline(-0.5, 1)
arrows(2, 1, 2, 1.5)
arrows(2, 2, 2, 1.5)
arrows(4, 4, 4, 3.5)
arrows(4, 3, 4, 3.5)

#f - The 7th observation is well into the blue territory, well outside the margin. 

#g - 

plot(data$x1,data$x2, col = colors)
abline(-.97,1)

# equation for the hyperplane is -0.97 + x1 + x2 = 0

plot(data$x1,data$x2, col = colors)
abline(-0.5,1)
points(c(2),c(2.5), col = c("blue"))

#7

#a

library(ISLR)

data.auto <- Auto
View(data.auto)
mpg.median = median(data.auto$mpg)

data.auto$high.mpg = ifelse(data.auto$mpg > mpg.median, 1, 0)
data.auto$mileage = as.factor(data.auto$high.mpg)
summary(data.auto$mileage)

data.auto <- subset(data.auto, select = -c(high.mpg) )

library(e1071)

svmfita =svm(data.auto$mileage ~., data=data.auto , kernel ="linear", cost=10, scale=FALSE)
summary(svmfita)
svmfita$index

set.seed(1)
tune.out=tune(svm,mileage~.,data=data.auto,kernel="linear", ranges = 
                  list(cost=c(0.001,0.01,0.1,1,5,10,100)))

summary(tune.out)

#cross validation is minimized at cost 1 with error = 0.01025641. 

#c 

set.seed(1)
tune.outp=tune(svm,mileage~.,data=data.auto,kernel="polynomial", ranges = 
                  list(cost=c(0.001,0.01,0.1,1,5,10,100), degree = c(2, 3, 4)))

summary(tune.outp)

#error is minimized at cost = 100 and degree = 2 with a error = 0.3013462 

set.seed(1)
tune.outr=tune(svm,mileage~.,data=data.auto,kernel="radial", ranges = 
                   list(cost=c(0.001,0.01,0.1,1,5,10,100), gamma = c(0.01, 0.1, 1, 5, 10)))

summary(tune.outr)

#error is minimized at cost = 100 and gamma = 0.01 with error = 0.01282051

#d

svm.linear <- svm(mileage ~., data=data.auto , kernel ="linear", cost=1, scale=FALSE)

plot(svm.linear,data = data.auto, acceleration ~ weight)
plot(svm.linear, data = data.auto, horsepower ~ weight)
plot(svm.linear, data = data.auto, year ~ horsepower)

svm.polynomial <- svm(mileage ~., data=data.auto , kernel ="polynomial", cost=100, degree = 2, scale=FALSE)

plot(svm.polynomial,data = data.auto, acceleration ~ weight)
plot(svm.polynomial, data = data.auto, horsepower ~ weight)
plot(svm.polynomial, data = data.auto, year ~ horsepower)

svm.radial <- svm(mileage ~., data=data.auto , kernel ="radial", cost=100, gamma = .01, scale=FALSE)

plot(svm.radial,data = data.auto, acceleration ~ weight)
plot(svm.radial, data = data.auto, horsepower ~ weight)
plot(svm.radial, data = data.auto, year ~ horsepower)

rm(list=ls())
options(scipen = 999)

setwd("C:/R Studio Files/POLS6394-Machine-Learning/Problem Set 5")



#Chapter 10 

#2

matrix = as.dist(matrix(c(0, 0.3, 0.4, 0.7, 
                     0.3, 0, 0.5, 0.8,
                     0.4, 0.5, 0.0, 0.45,
                     0.7, 0.8, 0.45, 0.0), nrow=4))

#a 

plot(hclust(matrix, method="complete"))

#b

plot(hclust(matrix, method="single"))

#c 

#cluster a - 1,2   cluster b - 3,4

#d

#cluster a - 1,2,3 cluster b - 4

#3

x1 <- c(1,1,0,5,6,4)
x2 <- c(4,3,4,1,2,0)

data <- as.data.frame(cbind(x1,x2))
View(data)

#a

plot(data$x1,data$x2)

#b

data$labels = sample(2, 6, replace=T)
data$labels


#c

NonDemocracies2016 <- NonDemocracies[ which(NonDemocracies$year >= 2003), ]

cluster.one <- data[which (data$labels==1), ]
cluster.two <- data[which (data$labels!=1), ]

centroid.one <- c(mean(cluster.one$x1), mean(cluster.one$x2))
centroid.one

centroid.two = c(mean(cluster.two$x1), mean(cluster.two$x2))
centroid.two

#d

distance <- function (x, y){
    return(sqrt((x[1] - y[1])^2 + (x[2] - y[2])^2))
}


distance(data, centroid.one)
distance(data, centroid.two)

#1 is closer to centroid.one
#2 is closer to centroid.one
#3 is closer to centroid.one
#4 is closer to centroid.two
#5 is closer to centroid.two
#6 is closer to centroid.two



#f

data$color <- ifelse(data$labels == 1,"Red","Blue")

plot(data$x1, data$x2, col = data$color, pch = 20, cex = 2)
points(centroid.one[1], centroid.one[2], col = "Red", pch = 4)
points(centroid.two[1], centroid.two[2], col = "Blue", pch = 4)

#9

library(ISLR)
set.seed(75)

data.arrests <- USArrests

#a

cluster1 <- hclust(dist(data.arrests), method="complete")
plot(cluster1)

#b

cutree(cluster1, 3)

table(cutree(cluster1, 3))

#c

scaled <- scale(data.arrests)
cluster2 <- hclust(dist(scaled), method = "complete")
plot(cluster2)

cutree(cluster2, 3)
table(cutree(cluster2,3))

#d 

#It's difficult to interpret much difference from the plot. The table of groupings with the cuts
#into 3 distinct clusters do show a difference, with more in the first cluster without scaling and
#more in the third cluster with scaling. I would scale the data because the units, and standard 
#deviations, for the different variables are not comparable. Making them more comparable should at least 
#improve intertepration and may eliminate some bias. 

