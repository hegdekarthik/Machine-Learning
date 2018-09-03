library(readxl)
Concrete <- read_excel("Downloads/Concrete_Data.xlsx")
View(Concrete)

boxplot(Concrete$Cement)
boxplot(Concrete$BlastFurnaceSlag)
boxplot(Concrete$FlyAsh)
boxplot(Concrete$Water)
boxplot(Concrete$Superplasticize)
boxplot(Concrete$CoarseAggregate)
boxplot(Concrete$FineAggregate)
boxplot(Concrete$Age)

plot(Concrete)
library(car)
#Co-variance of all variables
round(cor(Concrete),2)
#Univarial Analysis
c.fit <- lm(CompStrength ~ Cement, data = Concrete)
summary(c.fit)
anova(c.fit)

all.fit <- lm(CompStrength ~ ., data = Concrete)
summary(all.fit)
anova(all.fit)
round(vif(all.fit),2)

all.fit1 <- lm(CompStrength ~ BlastFurnaceSlag+FlyAsh+Water+Superplasticize+CoarseAggregate+FineAggregate+Age, data = Concrete)
summary(all.fit1)
anova(all.fit1)
round(vif(all.fit1),2)
plot(all.fit1)


all.fit2 <- lm(CompStrength ~ Cement+BlastFurnaceSlag+FlyAsh+Water+Superplasticize+Age, data = Concrete)
summary(all.fit2)
anova(all.fit2)
round(vif(all.fit2),2)
plot(all.fit2)


#Answer for Cement question
all.fit3 <- lm(CompStrength ~ Cement+FlyAsh+Water+Superplasticize+CoarseAggregate+FineAggregate+Age, data = Concrete)
summary(all.fit3)
anova(all.fit3)
round(vif(all.fit3),2)
plot(all.fit3)