library(rpart)
library(rpart.plot)
dec_tree <- rpart(Survived~.-SibSp-Parch, data = trainTitanicData, minbucket=4, method = 'class')
p_v <- predict(dec_tree, type='class')
table(trainTitanicData$Survived, p_v)
tp1 <- predict(dec_tree, newdata = testTitanicData, type = 'class')
table(tp1)
confusionMatrix(p_v,actual)
precision(p_v, actual)

library(randomForest)
rf_model <- randomForest(Survived~., data = trainTitanicData, ntree=2000, mtry=3,)
pval <- predict(rf_model)
actual <- trainTitanicData$Survived
table(actual, pval)
tp2 <- predict(rf_model, newdata = testTitanicData)
table(tp2)
confusionMatrix(pval,actual)
precision(pval, actual)

#k-fold Cross validation to find the cp value
numfold = trainControl(method = "cv", number = 10)    
cpgrid = expand.grid(.cp=seq(0.01,0.7,0.01)) 
cpVal = train(Survived~., data = trainTitanicData, method='rpart', trControl=numfold, tuneGrid=cpgrid)
cpVal

#Based on cpVal, replace cp in the decision tree model
dec_tree_2 <- rpart(Survived~., data = trainTitanicData, cp=0.04, method = 'class')
pv2 <- predict(dec_tree_2,type = 'class')
table(trainTitanicData$Survived, pv2)
tp3 <- predict(dec_tree_2, newdata = testTitanicData, type = 'class')
table(tp3)
confusionMatrix(pv2,actual)
precision(pv2, actual)

#k cross validation for RF
numfold = trainControl(method = "cv", number = 10)    
cpgrid = expand.grid(mtry=seq(1,10,1)) 
cpVal = train(Survived~., data = trainTitanicData, method='rf', trControl=numfold, tuneGrid=cpgrid)
cpVal

#Predict and check for the train value
predictTrain <- predict(rfModel)
actualTrain <- trainTitanicData$Survived
table(predictTrain,actualTrain)

#Build a Confusion Matrix and check the different parameters
confusionMatrix(predictTrain, actualTrain)

#Check the precsion of the preicted data
#84.1%
precision(predictTrain, actualTrain)

#Predict on Test data
predictTest <- predict(rfModel, newdata = testTitanicData)
table(predictTest)
rfModel <- randomForest(Survived~., data = trainTitanicData, ntree=2000, mtry=5)