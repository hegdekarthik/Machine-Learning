rm(list = ls())
library(readxl)
German_Credit <- read_excel("Documents/German Credit.xlsx", 
                            sheet = "All Data")
View(German_Credit)

German_Credit$ForeignWorker <- factor(German_Credit$ForeignWorker)
German_Credit$Telephone <- factor(German_Credit$Telephone)

str(German_Credit)

#Model Selection

IntOnly <- glm(Creditability ~1, data = German_Credit, family = binomial(link = "logit"))
fit6.lr <- glm(Creditability ~ ., data = German_Credit, family = binomial(link = "logit"))
bckCredit <- step(fit6.lr)
summary(bckCredit)

#Final Model (Dummy)

fit.dummy <- glm(Creditability ~ AcctBalance + DurCredit + `Paymnt Status` + Value + 
  Guarantors + Instalment + SexMS + LengthEmpl + ConcurrentCredits + 
  CreditAmt + ForeignWorker + Telephone + Apt + MVAA , data = German_Credit, family = binomial(link = "logit"))
summary(fit.dummy)

fwdCredit <- step(IntOnly, scope = list(lower=formula(IntOnly),upper=formula(fit6.lr)), direction = "forward")
summary(fwdCredit)
library(pROC)
roc(German_Credit$Creditability, fit.dummy$fitted.values)
plot.roc(German_Credit$Creditability, fit.dummy$fitted.values)

#STEP WISE
bothCredit = step(IntOnly, list(lower=formula(IntOnly),upper=formula(fit6.lr)),
                  direction="both")
summary(bothCredit)
fin.model <- glm(Creditability ~ AcctBalance + DurCredit + `Paymnt Status` + Value + 
                   Guarantors + Instalment + SexMS + LengthEmpl + ConcurrentCredits + 
                   CreditAmt + ForeignWorker + Telephone + Apt + MVAA + NoCredit,, data = German_Credit, family = binomial(link = "logit"))
summary(fin.model)

library(DescTools)
PseudoR2(fin.model)


####### Model Training ########
data(German_Credit)
