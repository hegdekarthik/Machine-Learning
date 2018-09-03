
iris_dataset = iris
View(iris_dataset)

set.seed(1)
n = nrow(iris_dataset)
trainIndex = sample(1:n, size = 0.85*n, replace = FALSE)
trainIris = iris_dataset[trainIndex , ]
testIris = iris_dataset[-trainIndex, ]

library(rpart)
library(rpart.plot)

#decision tree
tree = rpart(Species ~ .-Id, data = trainIris)
prp(tree)

#Prediction
predictVal = predict(tree, newdata = testIris)
actualVal = testIris$Species
pval = ifelse(predictVal[,1] >0.33, "Iris-setosa", 
              ifelse(predictVal[,2] >= 0.33, "Iris-versicolor", "Iris-virginica"))

#Confusion Matrix
cmTable = table(actualVal, pval)
cmTable
