rm(list = ls())

credit_test_data = read.csv("/Users/karthikhegde/Documents/PGP-DSE/Credit_Risk_Test_data.csv", header = TRUE)
credit_train_data = read.csv("/Users/karthikhegde/Documents/PGP-DSE/Credit_Risk_Train_data.csv", header = TRUE)
View(credit_train_data)
summary(credit_test_data)
summary(credit_train_data)

#Remove one column
credit_test_data['X'] = NULL

#Make both the dataset homogeneous. Change the column name
names(credit_train_data)[names(credit_train_data) == 'outcome'] <- 'Loan_Status'

#To get the NA's columnwise
sapply(credit_train_data, function(x){sum(is.na(x))})
sapply(credit_test_data, function(x){sum(is.na(x))})

#To get the Blank values columnwise
sapply(credit_train_data, function(x){sum(x == "")})
sapply(credit_test_data, function(x){sum(x == "")})

#Combine the data
credit_data = rbind(credit_test_data,credit_train_data)

#Check for NULL and empty values
sapply(credit_data, function(x){sum(is.na(x))})
sapply(credit_data, function(x){sum(x == "")})

#Check the summary of combined data
summary(credit_data)
View(credit_data)

#Manupilation of columns
#Change credit history to factor value
credit_data$Credit_History = factor(credit_data$Credit_History)

#Change the Gender
credit_data$Gender[credit_data$Gender == ""] <- "Male"

#Change the Marital State
credit_data$Married[credit_data$Married == ""] <- "Yes"

#Change self employed state
credit_data$Self_Employed[credit_data$Self_Employed == ""] <- "No"

#Change Dependents
credit_data$Dependents[credit_data$Dependents == ""] <- 0

#Change the Loan AMount
hist(credit_data$LoanAmount,breaks = 50)
credit_data$LoanAmount

#Complete this #######
which(is.na(credit_data$Loan_Amount_Term))

#Check for the values of such Loan_Amount_Term rows which has NA's in LoanAmount
credit_data$Loan_Amount_Term[which(is.na(credit_data$LoanAmount))]

#Since all the missing values in LoanAmount has 360 days in Laon_Amount_term, check the 
#distribution of LoanAmount for the term 360 days.
credit_data$LoanAmount[credit_data$Loan_Amount_Term == 360]
hist(credit_data$LoanAmount[credit_data$Loan_Amount_Term == 360], breaks = 50)

#Check mean and Median. But since the graph is right skewed, it would be better if we choose median over mean
mean(credit_data$LoanAmount[credit_data$Loan_Amount_Term == 360], na.rm = TRUE)
median(credit_data$LoanAmount[credit_data$Loan_Amount_Term == 360], na.rm = TRUE)

#Replace NA's in Loan Amount with the median calculated above.
credit_data$LoanAmount[is.na(credit_data$LoanAmount)] <- median(credit_data$LoanAmount[credit_data$Loan_Amount_Term == 360], na.rm = TRUE)

#Check for values of such LoanAmount which has NA in Loan_Amount_Term

credit_data$Loan_Amount_Term[is.na(credit_data$Loan_Amount_Term)] <- median(credit_data$Loan_Amount_Term,na.rm = TRUE)

# Change credit history by running logistic regression on the column by excluding Loan_status and loan_id
#Logistic regression
set.seed(1)
n = nrow(credit_data)
trainIndexCH = which(is.na(credit_data$Credit_History))
trainCredithistory = credit_data[-trainIndexCH , ]
testredithistory = credit_data[trainIndexCH, ]


#Regression

log_model <- glm(Credit_History~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Property_Area,data = trainCredithistory, family = binomial(link = "logit"))

#Predict Credit History
pred_ch = predict(log_model, newdata = testredithistory, type = 'response')
test_ch_val = ifelse(pred_ch > .75,1,0)

credit_data$Credit_History[trainIndexCH]<-test_ch_val

prop.table(table(credit_data$Gender))
prop.table(table(credit_data$Married))
prop.table(table(credit_data$Dependents))

# RUN THE DECISION TREE

cr_test = head(credit_data,n=nrow(credit_test_data))
cr_train = tail(credit_data, nrow(credit_train_data))
nrow(cr_train)

#Decision Tree
library(rpart)
library(rpart.plot)
library(caret)

tree=rpart(Loan_Status~ Gender+Married+Dependents+Education+Credit_History+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Property_Area,data=cr_train, minbucket=1)
prp(tree)
pd = predict(tree, newdata = cr_test)
pd_n = ifelse(pd[,2]>0.6,'Y','N')
table(cr_test$Loan_Status, pd_n)
actualData <- cr_test$Loan_Status

confusionMatrix(as.factor(pd_n),actualData)


#k-fold Cross validation to find the cp value
numfold = trainControl(method = "cv", number = 10)    
cpgrid = expand.grid(.cp=seq(0.01,0.7,0.01)) 
cpVal = train(Loan_Status~ Gender+Married+Dependents+Education+Credit_History+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Property_Area, data = cr_train, method='rpart', trControl=numfold, tuneGrid=cpgrid)
cpVal

#Based on cpVal, replace cp in the decision tree model
dec_tree_2 <- rpart(Loan_Status~ Gender+Married+Dependents+Education+Credit_History+Self_Employed+ApplicantIncome+CoapplicantIncome+LoanAmount+Loan_Amount_Term+Property_Area, data = cr_train, cp=0.44, method = 'class')
pv2 <- predict(dec_tree_2,newdata = cr_test, type = 'class')
table(as.factor(cr_test$Loan_Status), pv2)

confusionMatrix(pv2,actualData)
precision(pv2, actual)

#CONCLUSION: Both, decision tree and K-cross validation decision tree yields the same result.