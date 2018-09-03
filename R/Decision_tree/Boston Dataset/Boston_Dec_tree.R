rm(list = ls())
library(MASS)
library(rpart)
library(rpart.plot)

Boston$luxury_house = ifelse(Boston$medv >= mean(Boston$medv),1,0)
head(Boston)

set.seed(1)
n = nrow(Boston)
trainIndex = sample(1:n, size = 0.85*n, replace = FALSE)
trainBoston = Boston[trainIndex , ]
testBoston = Boston[-trainIndex, ]

acc_train = c()
acc_test = c()
for (var in seq(1,40,1)) {
  
  #TEST DATA
  tree_loop = rpart(luxury_house ~ .-medv, data = trainBoston, minbucket = var, method = "class")
  pred_test = predict(tree_loop, newdata = testBoston, type="class")
  #pred_test = factor(ifelse(pred_test[,2] >= 0.5, 1, 0))
  actual_test = factor(testBoston$luxury_house)
  
  table_cm = table(actual_test, pred_test)
  print (table_cm)
  accuracy_test = (table_cm[1,1] + table_cm[2,2])/nrow(testBoston)
  acc_test = c(acc_test,accuracy_test)
  #print(acc_test)
  
  #FOR TRAIN DATA
  tree_train = rpart(luxury_house ~ .-medv, data = trainBoston, minbucket = var, method = "class")
  pred_train = predict(tree_loop, newdata = trainBoston, type="class")
  actual_train = factor(trainBoston$luxury_house)
  
  table_cm_train = table(actual_train, pred_train)
  print (table_cm_train)
  accuracy_train = (table_cm_train[1,1] + table_cm_train[2,2])/nrow(trainBoston)
  acc_train = c(acc_train,accuracy_train)
  
}

print(acc_test)
print(acc_train)


plot(x=seq(1,40,1), y=acc_test, type = "l", col="red")
plot(x=seq(1,40,1), y=acc_train, type = "l", col="green")

