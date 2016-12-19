library(MASS)
install.packages("ISLR")

### Simple linear regression

names(Boston)

library(ggplot2)

#plotar o grafico de dispersão
ggplot(Boston, aes(lstat,medv)) + geom_point() + geom_line()

#calcular coeficientes de regressão linear
fit1=lm(medv~lstat,data=Boston)
summary(fit1)
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval='confidence')

#plotar a linha da regressão
ggplot(Boston, aes(lstat,medv)) + geom_point() + stat_smooth(method="lm",col="red")

### Multiple linear regression
fit2 = lm(medv~lstat+age,data=Boston)
summary(fit2)

fit3 = lm(medv~.,Boston) #todas as outras variáveis do dataset
summary(fit3)
fit4 = update(fit3,~.-age-indus)
summary(fit4)

#testar residuals

### Interactions
fit5 = lm(medv~lstat*age,Boston)
summary(fit5)


### Nonlinear relation
fit6 = lm(medv~lstat +I(lstat^2),Boston)
summary(fit6)
fitted(fit6)

ggplot(Boston, aes(lstat,medv)) + geom_point() + stat_smooth(method="lm",col="red",formula=y~poly(x,2)) + stat_smooth(method="lm",col="blue",formula=y~poly(x,4))

### Qualitative predictors
names(Carseats)
summary(Carseats)
fit1 = lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
contrasts(Carseats$ShelveLoc)





