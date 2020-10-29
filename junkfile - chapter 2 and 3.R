
#Problem 9(g)

Auto <- read.csv("C:/R Studio Files/POLS6394-Machine-Learning/datasets/Auto.csv")
View(Auto)


AutoMatrix <- data.matrix(Auto, rownames.force = NA)

pairs(AutoMatrix)

cor(AutoMatrix)

attach(Auto)

Auto$hp.num <- as.numeric(Auto$horsepower)

model1 <- lm(mpg ~ year*as.factor(name))
summary(model1)

model2 <- lm(mpg ~ as.factor(name))
summary(model2)

model3 <- lm(mpg ~ cylinders*displacement*Auto$hp.num*weight*acceleration*year*origin)

summary(model3)

model4 <- lm(mpg ~ cylinders*displacement*hp.num*weight*acceleration*year*origin + as.factor(name), data = Auto)
summary(model4)

model5 <- lm(mpg ~ cylinders + displacement + Auto$hp.num + weight + acceleration + year + origin + as.factor(name))
summary(model5)

model6 <- lm(mpg ~ cylinders*displacement*horsepower*weight*acceleration*year*origin)

summary(model6)

##country-year compares to model-year or origin-year
modelcy <- lm(mpg ~ year + as.factor(name))
summary(modelcy)

modeloy <- lm(mpg ~ year + origin)
summary(modeloy)

modelwa <- lm(mpg ~ weight*acceleration)
summary(modelwa)

modelh <- lm(mpg ~ horsepower)
summary(modelh)

modelhn <- lm(mpg ~ Auto$hp.num)
summary(modelhn)
