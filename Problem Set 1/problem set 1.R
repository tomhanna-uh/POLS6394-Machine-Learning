
gc()
rm(list=ls())
options(scipen = 999)


# Chapter 3 Lab: Linear Regression
setwd("C:/R Studio Files/POLS6394-Machine-Learning/Lab 3")
library(MASS)
library(ISLR)


Auto2 <- subset(Auto, select = -name)
cor(Auto2)

##Problem 13

rm(list=ls())
options(scipen = 999)

set.seed(1735)

#a

x <- rnorm(100)

#b

eps <- rnorm(100,mean = 0,sd = sqrt(0.25))

#c

y <- -1 + 0.5*x + eps

#The vector length is 100. B_0 = -1 and B_1 = 0.5

#d

plot(x,y)

#There is a positive linear relationship between x and y, with what appears to be a normal distribution and no outliers, as expected.

#e

model13e <- lm(y ~ x)
summary(model13e)

#The intercept, B_0, is fairly close to the original equation at -1.06. The B_1 coefficient, 0.4, is different than expected. Both coefficients are statistically significant.

#f


plot(x, y)
abline(model13e, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e", "Original equation"), col=2:1, lwd=5)

#g 
x2 <- x^2

model13g <- lm(y ~ x + x2)
summary(model13g)

plot(x, y)
abline(model13e, lwd=5, col=3)
abline(model13g, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e","Model 13g", "Original equation"), col=3:1, lwd=5)



#The R-squared, regular and adjusted, for the polynomial model is slightly lower than the simpler model.
#The fitted lines are similar. The effect of X^2 is small in magnitude and not significant.

confint(model13e)

#h

rm(list=ls())
options(scipen = 999)

set.seed(1735)

#a

x <- rnorm(100)

#b

eps <- rnorm(100,mean = 0,sd = sqrt(0.05))

#c

y <- -1 + 0.5*x + eps

#The vector length is 100. B_0 = -1 and B_1 = 0.5

#d

plot(x,y)

#There is a positive linear relationship between x and y, with what appears to be a normal distribution and no outliers, as expected.

#e

model13he <- lm(y ~ x)
summary(model13he)

#The intercept, B_0, is closer to the original equation at -1.03. The B_1 coefficient, 0.45, is different than expected, but closer than in the first simulation. Both coefficients are statistically significant.

#f


plot(x, y)
abline(model13he, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e", "Original equation"), col=2:1, lwd=5)

#g 
x2 <- x^2

model13hg <- lm(y ~ x + x2)
summary(model13hg)

plot(x, y)
abline(model13he, lwd=5, col=3)
abline(model13hg, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e","Model 13g", "Original equation"), col=3:1, lwd=5)



#The multiple R-squared is higher, but the adjusted R-squared is lower in the polynomial model. The Residual Standard Error is higher in the polynomial model. The X^2 term is not significant. 
#The fitted lines are nearly indistinguishable.

confint(model13he)


#i

rm(list=ls())
options(scipen = 999)

set.seed(1735)

#a

x <- rnorm(100)

#b

eps <- rnorm(100,mean = 0,sd = sqrt(0.75))

#c

y <- -1 + 0.5*x + eps

#The vector length is 100. B_0 = -1 and B_1 = 0.5

#d

plot(x,y)

#There is a positive linear relationship between x and y. There are no outliers, but the data is not well grouped.

#e

model13ie <- lm(y ~ x)
summary(model13ie)

#The intercept, B_0, at -1.10 is different than the original equation. The B_1 coefficient, 0.32, is much different than expected. Both coefficients are statistically significant.

#f


plot(x, y)
abline(model13ie, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e", "Original equation"), col=2:1, lwd=5)

#g 
x2 <- x^2

model13ig <- lm(y ~ x + x2)
summary(model13ig)

plot(x, y)
abline(model13ie, lwd=5, col=3)
abline(model13ig, lwd=5, col=2)
abline(-1, 0.5, lwd=5, col=1)
legend(-1, legend = c("Model 13e","Model 13g", "Original equation"), col=3:1, lwd=5)


#The multiple R-squared is better, but the adjusted R-squared is worse for the polynomial model. The RSE is worse for the polynomial model. The X^2 coefficient is not significant. 

confint(model13ie)

#j

#confint(model13e)
#                 2.5 %     97.5 %
#   (Intercept) -1.1464580 -0.9692968
#    x            0.3054538  0.4905429

#confint(model13he)
#                    2.5 %     97.5 %
#    (Intercept) -1.0654980 -0.9862691
#    x            0.4129963  0.4957707

#  confint(model13ie)
#              2.5 %     97.5 %
#(Intercept) -1.253673 -0.9468206
#x            0.163036  0.4836198

#The fit is best on the model with the least noise and widest on the model with the most noise.