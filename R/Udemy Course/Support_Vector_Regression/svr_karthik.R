# Regression Template

rm(list=ls())

# Importing the dataset
dataset = read.csv('Position_Salaries.csv')
dataset = dataset[2:3]

#Create SVR Regressor
library(e1071)
regressor = svm(formula = Salary ~ ., 
                data = dataset,
                type = 'eps-regression')


#Prediction

y_pred = predict(regressor, data.frame(Level = 6.5))

# Visualising the SVM Regression results

library(ggplot2)
ggplot() +
  geom_point(aes(x = dataset$Level, y = dataset$Salary),
             colour = 'red') +
  geom_line(aes(x = dataset$Level, y = predict(regressor, newdata = dataset)),
            colour = 'blue') +
  ggtitle('Truth or Bluff (SVR Regression)') +
  xlab('Level') +
  ylab('Salary')
