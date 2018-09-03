  
  rm(list = ls())
  library(MASS)
  
  Boston$luxury_house = ifelse(Boston$medv >= mean(Boston$medv),1,0)
  head(Boston)
  View(Boston)
  
  library(rpart)
  library(rpart.plot)
  
  #decision tree
  tree = rpart(luxury_house ~ .-medv, data = Boston)
  par(mfrow=c(1,1))
  prp(tree)
  
  predictVal = predict(tree, newdata = Boston)
  predictVal
  
  str(predictVal)
  
  predictVal = ifelse(predictVal >= 0.5, 1, 0)
  predictVal = factor(predictVal)
  Boston$predictVal = predictVal
  
  actualVal = factor(Boston$luxury_house)
  
  cmTable = table(actualVal, predictVal)
  cmTable
  library(caret)
  confusionMatrix(actualVal,predictVal)
  library(DescTools)
  Conf(actualVal,predictVal)
  summary(tree)
  
  #USING CLASS METHOD
  tree_2 = rpart(luxury_house ~ .-medv -predictVal, data = Boston, method = "class")
  prp(tree_2)
  par(mfrow=c(1,2))

  #K Cross Validation
  
  library(caret)
  library(e1071)
  Boston$luxury_house = as.factor(Boston$luxury_house)
  numfold = trainControl(method = "cv", number = 10)    
  cpgrid = expand.grid(.cp=seq(0.01,0.7,0.01))  
  
  asd = train(luxury_house~.-medv, data = Boston, method='rpart', trControl=numfold, tuneGrid=cpgrid)  

  tree_final = rpart(luxury_house ~ .-medv, data = Boston, cp= 0.04, method = 'class')  
  tree_final  
  pre = predict(tree_final,Boston)  
  pre = ifelse(pre[,2]>=0.5,1,0)  

  table(Boston$luxury_house,pre)
  acc = (274+173)/nrow(Boston)  
  
  ## RANDOM FOREST
  library(randomForest)
  rf = randomForest(luxury_house~.-medv, data = Boston)  
  pr_rf = predict(rf)  
  table(Boston$luxury_house, pr_rf)  
  acc_rf = (275+176)/nrow(Boston)
  
  #with mtry
  rf_ = randomForest(luxury_house~.-medv, data = Boston, ntree=1000, mtry=3)
  pr_rf_ = predict(rf_) 
  table(Boston$luxury_house, pr_rf_)
  accrf_ = (275+178)/nrow(Boston)
  
  #importance of variable
  varImp(rf)
  varImpPlot(rf)
  summary(rf)  
  