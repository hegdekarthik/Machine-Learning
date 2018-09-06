# Data Preprocessing

# Importing the dataset
dataset = read.csv('Salary_Data.csv')

#Split the train and test data
set.seed(12)
n = nrow(dataset)
trainIndex = sample(1:n,size=(2/3)*n, replace=FALSE)
trainData <- dataset[trainIndex,]
testData <- dataset[-trainIndex,]

#Simple Linear Regression
regressor = lm(Salary ~ ., data = trainData)
summary(regresson)

#Predict the data
pred = predict(regressor, newdata = testData)
pred_train = predict(regressor)
pred

#Visualization
library(ggplot2)
ggplot() + 
  geom_point(aes(x=trainData$YearsExperience , y=trainData$Salary),color = 'red') +
  geom_line(aes(x = trainData$YearsExperience, y=pred_train), color = 'blue')+
ggtitle('Salary vs Experience (Train set)') +
  xlab('Years of experience') +
  ylab('Salary')

#Test Set
ggplot() + 
  geom_point(aes(x=testData$YearsExperience , y=testData$Salary),color = 'red') +
  geom_line(aes(x = trainData$YearsExperience, y=pred_train), color = 'blue')+
  ggtitle('Salary vs Experience (Test set)') +
  xlab('Years of experience') +
  ylab('Salary')
