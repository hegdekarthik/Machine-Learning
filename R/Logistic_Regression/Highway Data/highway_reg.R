#Please read 'readme' file to find more about the dataset.

rm(list = ls())

library(readxl)
library(corrplot)
library(leaps)

Highway <- read_excel("Downloads/Highway.xlsx", sheet = "Sheet4")
View(Highway)
str(Highway)

#By looking at the data sheet and reference sheet, we can say that 'Hwy' is a categorical variable
# and remaining are continuous variable.

#Convert 'Hwy' to a factor variable.
Highway$Hwy = factor(Highway$Hwy)
summary(Highway)

#Since there are just 2 '0' values, club it along with '1'.
Highway$Hwy[Highway$Hwy == 0] <- 1
summary(Highway)

#Since we can't have a correlation matrix for categorical value, we shouldn't consider it
#Hence neglecting 'Hwy', which is 11th column of dataframe and rounding it off for the 2nd decimal.

corHighway <- round(cor(Highway[,-11],Highway[,-11]),2)
corrplot(corHighway, method="number")

#Divide the dataset into train data and test data before fitting a regression to the data
#Divide the data into 70:30 ratio.
set.seed(45)
n = nrow(Highway)
trainIndex = sample(1:n, size = round(0.7*n), replace = FALSE)

trainHighway = Highway[trainIndex , ]
testHighway = Highway[-trainIndex , ]

#Prediction/fitting the linear curve for the data
#Instead of using univariate analysis let's go ahead and find what's the best model using
#Forward Selection

#lm is the 'Linear Regression' model used. 
#null contains 0 variable model
null=lm(Rate~1, data=trainHighway)

#full.fit contains the model including all the variables
full.fit=lm(Rate~., data=trainHighway)

#Forward selection analysis
forward.fit <- step(null, scope=list(lower=null, upper=full.fit), direction="forward")
summary(forward.fit)
round(vif(forward.fit),2)

#Model obtained by forward selection

#Call:
#  lm(formula = Rate ~ Acpt + Trks + Len, data = trainHighway)
#
#Coefficients:
#  (Intercept)         Acpt         Trks          Len  
#5.48809      0.11421     -0.20414     -0.06372 


#Analysis in Both the Direction
both.fit <- step(null, scope=list(lower=null, upper=full.fit), direction="both")
summary(both.fit)

#Model obtained by both direction selection

#Call:
#  lm(formula = Rate ~ Acpt + Trks + Len, data = trainHighway)
#
#Coefficients:
#  (Intercept)         Acpt         Trks          Len  
#5.48809      0.11421     -0.20414     -0.06372 

#Regression Subset
subset.fit=regsubsets(Rate~., data=trainHighway, nbest=3)
summary(subset.fit)
#Plot AIC Graph to select the number of predictors 
plot(subset.fit, scale="adjr2")

#Plot BIC Graph to select the number of predictors 
plot(subset.fit, scale="bic")

reg.fit <- lm(Rate ~ Trks + Acpt + Hwy, data = trainHighway)


#Now, let's look into summary of all the models. Depending on the various criteria, we'll have to 
#select the best model

summary(reg.fit)
anova(reg.fit)
vif(reg.fit)
#In reg.fit, with both summary and anova table we can clearly see that 'Hwy' varibale is not highly significant as it's p value > 0.0.5
# and even though R-Square is bit higher than both.fit, adjusted R-Square is almost same.

#Even F-Stat is bit higher for 'both.fit'.
summary(both.fit)
anova(both.fit)
vif(both.fit)

#Let's go ahead and predict the data on testdata and see which model is the best
both.predicted <- predict.lm(both.fit, newdata=testHighway)

reg.predicted <- predict.lm(reg.fit, newdata=testHighway)

#Final answer/best model for this data would be
final.model <- lm(formula = Rate ~ Acpt + Trks + Len, data = trainHighway)
plot(final.model)