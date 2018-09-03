#Question 2 

rm(list = ls()) 

#Read the data from the file with the headers
cmc.data<- read.table("cmc.data",header = FALSE, sep = ",", col.names = c('wife_age','wife_education','husband_education','no_child','wife_relegion','wife_working','husband_occpn','sol_index','media_exposure','contr_used'))
view(cmc)

cols <- c('wife_education','husband_education','wife_relegion','wife_working','husband_occpn','sol_index','media_exposure')

#Combine target varibale 'contr_used'. Combine 1 & 2
cmc.data$contr_used[cmc.data$contr_used == 2] <- 1
cmc.data$contr_used[cmc.data$contr_used == 1] <- 0
cmc.data$contr_used[cmc.data$contr_used == 3] <- 1

#Convert all the columns to factor variables
cmc.data[cols] = lapply(cmc.data[cols], factor)

#View the datatypes of all columns 
str(cmc.data)

#Summary of the data
summary(cmc.data)

#Convert the dataset into test and train data
set.seed(108)
n = nrow(cmc.data)
trainIndex = sample(1:n, size = 0.7*n, replace = FALSE)
trainCmc = cmc.data[trainIndex , ]
testCmc = cmc.data[-trainIndex, ]

IntOnly <- glm(contr_used ~1, data = trainCmc, family = binomial(link = "logit"))
full.fit <- glm(contr_used ~ ., data = trainCmc, family = binomial(link = "logit"))
back.fit <- step(full.fit, direction = "backward")
summary.glm(back.fit)

library(pROC)
cmcRoc = roc(trainCmc$contr_used, back.fit$fitted.values)
cmcRoc
plot(cmcRoc)

library(DescTools)
PseudoR2(back.fit)

library(ResourceSelection)
hoslem.test(trainCmc$contr_used, back.fit$fitted.values, g=10)

#Forward Selection
forward.fit <- step(IntOnly, scope = list(lower=formula(IntOnly),upper=formula(full.fit)), direction = "forward")
summary.glm(forward.fit)
PseudoR2(forward.fit)

#Both Fit
both.fit <- step(IntOnly, scope = list(lower=formula(IntOnly),upper=formula(full.fit)), direction = "both")
summary.glm(both.fit)


#Predict Data forward
predictCmc <- predict(forward.fit, newdata = testCmc, type = "response")
preictContrUsed <- ifelse(predictCmc > 0.5, 1 , 0)
preictContrUsed <- factor(preictContrUsed, levels = c(0,1))
contrCol <- testCmc$contr_used
mean(preictContrUsed == contrCol) #68.8

qw = table(preictContrUsed,testCmc$contr_used)
cmTable = table(preictContrUsed, testCmc$contr_used)
library(caret)
confusionMatrix(qw)



## Part 2
rm(list = ls())
cmc.data2 <- read.table("cmc.data",header = FALSE, sep = ",", col.names = c('wife_age','wife_education','husband_education','no_child','wife_relegion','wife_working','husband_occpn','sol_index','media_exposure','contr_used'))

cols <- c('wife_education','husband_education','wife_relegion','wife_working','husband_occpn','sol_index','media_exposure')

cols_1 <- c('wife_age','wife_education','husband_education','no_child','wife_relegion','wife_working','husband_occpn','sol_index','media_exposure','contr_used')
#Combine target varibale 'contr_used'. Combine 1 & 2
cmc.data2$contr_used[cmc.data2$contr_used == 1] <- 0
cmc.data2$contr_used[cmc.data2$contr_used == 2] <- 1
cmc.data2$contr_used[cmc.data2$contr_used == 3] <- 1

#Convert all the columns to factor variables
cmc.data2[cols] = lapply(cmc.data2[cols], factor)

#View the datatypes of all columns 
str(cmc.data2)

#Convert the dataset into test and train data
set.seed(108)
n = nrow(cmc.data2)
trainIndex2 = sample(1:n, size = 0.7*n, replace = FALSE)
trainCmc2 = cmc.data2[trainIndex2 , ]
testCmc2 = cmc.data2[-trainIndex2, ]

IntOnly2 <- glm(contr_used ~1, data = trainCmc2, family = binomial(link = "logit"))
full.fit2 <- glm(contr_used ~ ., data = trainCmc2, family = binomial(link = "logit"))
back.fit2 <- step(full.fit2, direction = "backward")
summary.glm(back.fit2)

library(pROC)
roc(trainCmc2$contr_used, back.fit2$fitted.values)

library(DescTools)
PseudoR2(back.fit2)

library(ResourceSelection)
hoslem.test(trainCmc2$contr_used, back.fit2$fitted.values, g=10)

#Forward Selection
forward.fit2 <- step(IntOnly2, scope = list(lower=formula(IntOnly2),upper=formula(full.fit2)), direction = "forward")
summary.glm(forward.fit2)
PseudoR2(forward.fit2)
cmcRoc2 = roc(trainCmc2$contr_used, forward.fit2$fitted.values)

cmcRoc2
plot(cmcRoc2)

#Both Fit
both.fit2 <- step(IntOnly2, scope = list(lower=formula(IntOnly2),upper=formula(full.fit2)), direction = "both")
summary.glm(both.fit2)

head(forward.fit2$fitted.values)

#Predict Data forward
predictCmc2 <- predict(forward.fit2, newdata = testCmc2, type = "response")
preictContrUsed2 <- ifelse(predictCmc2 > 0.5, 1 , 0)
actualcontrCol2 <- testCmc2$contr_used

#Get the Percentage of corect value for the model
mean(preictContrUsed2 == actualcontrCol2)

#Develop the confusion matrix
cmTable = table(preictContrUsed2, testCmc2$contr_used)
qw = table(preictContrUsed2,testCmc2$contr_used)
library(caret)
confusionMatrix(qw)
length(testCmc2$contr_used[testCmc2$contr_used == 0])
