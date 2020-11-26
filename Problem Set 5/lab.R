set.seed(1) 
x=matrix(rnorm (20*2) , ncol=2) 
y=c(rep(-1,10), rep (1,10)) 
x[y==1,]= x[y==1,] + 1 
plot(x, col =(3-y))

dat=data.frame(x=x, y=as.factor(y))
library (e1071)
svmfit1 =svm(y ~., data=dat , kernel ="linear", cost=10, scale=FALSE)
summary(svmfit1)

plot(svmfit1 , dat)
svmfit1$index

svmfit1 =svm(y ~., data=dat , kernel ="linear", cost=0.1, scale=FALSE)
plot(svmfit2 , dat)
svmfit$index

set.seed(1)
tune.out=tune(svm ,y ~.,data=dat ,kernel ="linear", ranges =list(cost=c(0.001, 0.01, 0.1, 1,5,10,100) ))
tune.out