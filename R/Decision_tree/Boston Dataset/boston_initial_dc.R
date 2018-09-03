library(MASS)
View(Boston)

library(rpart)
library(rpart.plot)
tree = rpart(medv ~ ., data = Boston)
prp(tree)


pvl = predict(tree, newdata = Boston)

#RMSE VALUE

rmse_val = sum((Boston$medv - pvl)^2)/nrow(Boston)
rmse_val = sqrt(rmse_val)

#MAPE VALUE

mape_val = sum(abs((pvl - Boston$medv))/Boston$medv)
mape_val = mape_val/nrow(Boston)

#MAPE and RMSE using formulas
library(DescTools)
RMSE(x =pvl,ref = Boston$medv)
MAPE(x =pvl,ref = Boston$medv)

##Day 2 with changing minbucket values nrow(Boston)
