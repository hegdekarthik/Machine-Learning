rm(list=ls())

library(readxl)
PaulBooks1 <- read_excel("Documents/PaulBooks1.xlsx", 
                         sheet = "PaulBooks1")
View(PaulBooks1)
str(PaulBooks1)

### Model Training ###
data(PaulBooks1)
n = nrow(PaulBooks1)
set.seed(28)

trainIndex = sample(1:n, size = round(0.7*n), replace = FALSE)
trainIndex

trainPaul = PaulBooks1[trainIndex , ]
testPaul = PaulBooks1[-trainIndex , ]

nrow(trainPaul[trainPaul$Purchase == 1, ]) #56/700
nrow(testPaul[testPaul$Purchase == 1, ]) #27/300

#Select Model on Train Data
IntOnly <- glm(Purchase ~ 1,  data = trainPaul, family = binomial(link = "logit"))
fit.lr <- glm(Purchase ~ Months + NoBought, data = trainPaul, family = binomial(link = "logit"))
bothCredit = step(IntOnly, list(lower=formula(IntOnly),upper=formula(fit.lr)),
                  direction="both")

fin.model <- glm(Purchase ~ NoBought + Months, data = trainPaul, family = binomial(link = "logit"))
summary(fin.model)


#Predict Data
predictData <- predict(fin.model, newdata = testPaul, type = "response")
purchaseNumber <- ifelse(predictData > 0.5, 1 , 0)
buyNumber <- factor(purchaseNumber, levels = c(0,1))

purchaceCol <- testPaul$Purchase

mean(purchaceCol)
mean(purchaseNumber)

mean(buyNumber == purchaceCol)


