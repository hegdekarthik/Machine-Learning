rm(list = ls())


library(readxl)
Indian_Patients <- read_excel("Downloads/Indian Liver Patient Dataset.xlsx", 
                              sheet = "Indian Liver Patient Dataset (I")
View(Indian_Patients)
Indian_Patients$Sex <- factor(Indian_Patients$Sex)

summary(Indian_Patients)
cor(Indian_Patients[,-2])
Indian_Patients$Diagnosis[Indian_Patients$Diagnosis == 2] <- 0

fit1.lr1 <- glm(Diagnosis ~ Age + TB + AAP + SAA1  +  AGRatio + Diagnosis, data = Indian_Patients, family=binomial(link=logit))
summary.glm(fit1.lr1)
anova(fit1.lr1)

library(car)
vif(fit1.lr1)
library(pROC)

roc(Indian_Patients$Diagnosis, fit1.lr1$fitted.values)
plot.roc(Indian_Patients$Diagnosis, fit1.lr1$fitted.values)

tab1 = with(Indian_Patients,(table(Diagnosis, fit1.lr1$fitted.values > 0.5)))

#Diagnosis FALSE TRUE
#0           40   127
#1           27   389

#TPR = 389/416

#TNR = TN/N = 42/167


## DAY 2 CODE ##

IntOnly <- glm(Diagnosis ~1, data = Indian_Patients, family = binomial(link = "logit"))
fit6.lr <- glm(Diagnosis ~ ., data = Indian_Patients, family = binomial(link = "logit"))
bckCredit <- step(fit6.lr)
summary(bckCredit)

#backward
m1 <- glm(Diagnosis ~ Age + DB + AAP + SAA1 + TP + ALB + AGRatio, data = Indian_Patients, family = binomial(link = "logit"))
summary(m1)
library(car)
vif(m1)
PseudoR2(m1)
hoslem.test(Indian_Patients$Diagnosis, m1$fitted.values, g=10)
table_1 = with(Indian_Patients,(table(Diagnosis, m1$fitted.values > 0.5)))
table_1


m1_1 <- glm(Diagnosis ~ Age + DB  + SAA1 + TP + ALB , data = Indian_Patients, family = binomial(link = "logit"))
summary(m1_1)
library(pROC)
roc(Indian_Patients$Diagnosis, m1_1$fitted.values)

library(DescTools)
PseudoR2(m1_1)

library(ResourceSelection)
hoslem.test(Indian_Patients$Diagnosis, m1_1$fitted.values, g=10)

table_2 = with(Indian_Patients,(table(Diagnosis, m1_1$fitted.values > 0.5)))
table_2

#forward
fwdCredit <- step(IntOnly, scope = list(lower=formula(IntOnly),upper=formula(fit6.lr)), direction = "forward")
m2 <- glm(Diagnosis ~ DB + SAA1 + Age + AAP, data = Indian_Patients, family = binomial(link = "logit"))
summary(m2)
PseudoR2(m2)

hoslem.test(Indian_Patients$Diagnosis, m2$fitted.values, g=10)

tab_3 = with(Indian_Patients,(table(Diagnosis, m2$fitted.values > 0.5)))
tab_3

roc(Indian_Patients$Diagnosis, m2$fitted.values)

#stepwise
bothCredit = step(IntOnly, list(lower=formula(IntOnly),upper=formula(fit6.lr)),
                  direction="both")

m3 <- glm(Diagnosis ~ DB + SAA1 + Age + AAP, data = Indian_Patients, family = binomial(link = "logit"))
summary(m3)


## Model Training and testing ###
data(Indian_Patients)
n = nrow(Indian_Patients)

set.seed(45)

trainIndex = sample(1:n, size = round(0.8*n), replace = FALSE)
trainIndex

trainPat = Indian_Patients[trainIndex , ]
testPat = Indian_Patients[-trainIndex , ]

nrow(trainPat[trainPat$Diagnosis == 0, ]) #56/700
nrow(testPat[testPat$Diagnosis == 0, ])

nrow(trainPat[trainPat$Diagnosis == 1, ]) #56/700
nrow(testPat[testPat$Diagnosis == 1, ])

## Prediction Part ##
Int <- glm(Diagnosis ~1, data = trainPat, family = binomial(link = "logit"))
for.lr <- glm(Diagnosis ~ ., data = trainPat, family = binomial(link = "logit"))


fwdTrain <- step(Int, scope = list(lower=formula(Int),upper=formula(for.lr)), direction = "forward")
backTrain <- step(for.lr)

for.mod <- glm(Diagnosis ~ DB + Age + AGRatio + SAA1, data = trainPat, family = binomial(link = "logit"))
summary(for.mod)
roc(trainPat$Diagnosis, for.mod$fitted.values)

back.mod <- glm(Diagnosis ~ Age + DB + SAA1 + TP + ALB, data = trainPat, family = binomial(link = "logit"))
summary(back.mod)
roc(trainPat$Diagnosis, back.mod$fitted.values)


#Predict Data forward
predictData <- predict(for.mod, newdata = testPat, type = "response")
diagNumber <- ifelse(predictData > 0.5, 1 , 0)
diagNumber <- factor(diagNumber, levels = c(0,1))
diagCol <- testPat$Diagnosis

mean(diagNumber == diagCol)

#Predict Data backward
predictDataBack <- predict(back.mod, newdata = testPat, type = "response")
diagNumberBack <- ifelse(predictDataBack > 0.5, 1 , 0)
diagNumberBack <- factor(diagNumberBack, levels = c(0,1))

diagColBack <- testPat$Diagnosis

mean(diagNumber == diagCol)
