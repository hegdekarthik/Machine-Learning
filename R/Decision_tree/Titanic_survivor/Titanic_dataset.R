#Titanic Project

#Clear the workspace
rm(list = ls())

library(caret)
library(e1071)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)

#Read the data
trainTitanic = read.csv("Titanic/train.csv", header = TRUE)
testTitanic = read.csv("Titanic/test.csv", header = TRUE)

#datatypes
str(trainTitanic)
str(testTitanic)

#Fill Survived column of test data to NA to make it homogeneous with the test data
testTitanic$Survived = NA

#Combine the data
completeTitanic = rbind(trainTitanic, testTitanic)

#Check the datatypes of Titanic Data
str(completeTitanic)

#Convert the datatypes to required format
completeTitanic$Survived = factor(completeTitanic$Survived)
completeTitanic$Pclass = factor(completeTitanic$Pclass)
completeTitanic$Name = as.character(completeTitanic$Name)

#Check for null and missing values
sapply(completeTitanic[,-c(2)], function(x){sum(is.na(x))})
sapply(completeTitanic[,-c(2)], function(x){sum(x == "")})

#Fill the missing values accordingly
hist(completeTitanic$Fare, na.rm = TRUE)
summary(completeTitanic)

#Fill the 'Fare' with median value
histogram(completeTitanic$Fare, xlab="Fare", main="Histogram of Ticket's Fare", frequency=TRUE, ylab="Frequency")
completeTitanic$Fare[is.na(completeTitanic$Fare)] = median(completeTitanic$Fare, na.rm = TRUE)

#Fill the 'Embarked' with the mode, which is 'S'
completeTitanic$Embarked[completeTitanic$Embarked == ""] <- 'S'

# Create a new variable which contains siblings, spouse and individuals
completeTitanic$FamilySize <- completeTitanic$SibSp + completeTitanic$Parch + 1

#Process the data to split the Titles in the name
completeTitanic$Title <- sapply(completeTitanic$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
completeTitanic$Title <- sub(' ', '', completeTitanic$Title)

#Before finding/filling the age, combine the sir names to most common ones.
completeTitanic$Title <- as.character(completeTitanic$Title)
completeTitanic$Title[completeTitanic$Title %in% c('Dona', 'Lady', 'the Countess', 'Ms', 'Mme', 'Mlle')] <- 'Mrs'
completeTitanic$Title[completeTitanic$Title %in% c('Capt', 'Col', 'Don', 'Dr', 'Jonkheer', 'Major', 'Rev', 'Sir')] <- 'Others'


#Check the number of people with Titles
table(as.factor(completeTitanic$Title))

#Fill the ages with respect to their Title

#completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Mr')] <- median(completeTitanic$Age[completeTitanic$Title == "Mr"],na.rm = TRUE)
#completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Mrs')] <- median(completeTitanic$Age[completeTitanic$Title == "Mrs"],na.rm = TRUE)
#completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Master')] <- median(completeTitanic$Age[completeTitanic$Title == "Master"],na.rm = TRUE)
#completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Miss')] <- median(completeTitanic$Age[completeTitanic$Title == "Miss"],na.rm = TRUE)
#completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == 'Others')] <- median(completeTitanic$Age[completeTitanic$Title == "Others"],na.rm = TRUE)
tapply(is.na(completeTitanic$Age),completeTitanic$Title,sum)

for (i in unique(completeTitanic$Title)){
  #print(i)
  #print (median(completeTitanic$Age[completeTitanic$Title == i],na.rm = TRUE))
  completeTitanic$Age[which(is.na(completeTitanic$Age) & completeTitanic$Title == i)] <- median(completeTitanic$Age[completeTitanic$Title == i],na.rm = TRUE)
}


#Add one more Title for the people aged below 18
#completeTitanic$Title[completeTitanic$Age < 18] <- 'Children'
completeTitanic$Children[completeTitanic$Age < 18] <- 'Yes'
completeTitanic$Children[completeTitanic$Age >= 18] <- 'No'
completeTitanic$Title <- factor(completeTitanic$Title)
completeTitanic$Children <- factor(completeTitanic$Children)
levels(as.factor(completeTitanic$Title))
#Now remove the redundant columns which we will not be used in the prediction algorithm
columnToDelete <- c('Cabin', 'PassengerId', 'Name','Ticket')
completeTitanic[,columnToDelete] <- NULL

#Seperate Test and Train data from the combined data
trainTitanicData <- head(completeTitanic, n = nrow(trainTitanic))
testTitanicData <- tail(completeTitanic, n = nrow(testTitanic))

trainTitanicData$Title <- factor(trainTitanicData$Title)
testTitanicData$Title <- factor(testTitanicData$Title)

#Model building and prediction part

#Using Decision Tree and K cross validation

#Use K cross validation to find the 'cp' parameter
set.seed(1)
numfold = trainControl(method = "cv", number = 10)    
cpgrid = expand.grid(.cp=seq(0.01,0.7,0.01)) 
cpVal = train(Survived~., data = trainTitanicData, method='rpart', trControl=numfold, tuneGrid=cpgrid)
cpVal

#Based on cpVal, replace cp in the decision tree model
dec_tree_2 <- rpart(Survived~., data = trainTitanicData, cp=0.02, method = 'class')
prp(dec_tree_2)
pv2 <- predict(dec_tree_2,type = 'class')
table(trainTitanicData$Survived, pv2)

#Predict the value
pred_titanic <- predict(dec_tree_2, newdata = testTitanicData, type = 'class')
table(pred_titanic)

#write the output to output file
newdf_ <- data.frame(PassengerId = testTitanic$PassengerId, Survived = 0)
newdf_$Survived <- pred_titanic
write.csv(newdf_, 'DT_12.csv', row.names = F)



gP = ggplot(data = trainTitanicData, aes(x = `Sex`, y=`Age`))
gP + geom_bar(stat = "identity", colour = "lightblue")

g <- ggplot(data = trainTitanicData, aes(Survived, Embarked))
# Number of cars in each class:
g + geom_bar(stat = "identity", stat = "count",aes(fill = Sex))

ggplot(data = trainTitanicData, aes(Embarked, fill = Survived)) +
  geom_histogram(stat="count")

ggplot(data = trainTitanicData, aes(Age, color = Survived)) +
  geom_freqpoly(binwidth = 500)
