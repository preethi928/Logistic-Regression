creditcard<-read.csv("C:\\Data_science\\EXCLER\\My Assignments\\Logistic regression\\creditcard.csv")

str(creditcard)

x<-as.factor(creditcard$X)
reports<-as.factor(creditcard$reports)
dependents<-as.factor(creditcard$dependents)
months<-as.factor(creditcard$months)
majorcards<-as.factor(creditcard$majorcards)
active<-as.factor(creditcard$active)
cards<-as.factor(creditcard$card)

str(majorcards)#two levels

str(active)# i have checked for levels in all variables

str(creditcard)

#lets check for NA values
is.na(creditcard)
sum(is.na(creditcard))

#plotting

plot(creditcard)

#Lets check for outliers in boxplot
boxplot(creditcard$X)
#No outliers
boxplot(creditcard$card)#Not factor
boxplot(creditcard$reports)#too many outliers
boxplot(creditcard$age)
#too many outliers
boxplot(creditcard$income)
#too many outliers
boxplot(creditcard$share)
#too many outliers
boxplot(creditcard$expenditure)
#too many outliers
owner<-as.factor(creditcard$owner)
boxplot(creditcard$owner)#Not a factor
boxplot(creditcard$selfemp)#Not a factor
boxplot(creditcard$dependents)
#one outlier
boxplot(creditcard$months)
#too many outliers
boxplot(creditcard$majorcards)
#one outlier
boxplot(creditcard$active)
#too many outlier

#the variables used below are the only those which has two levels 
#GLM MODEL

cards<-glm(card ~ owner + selfemp + majorcards, family ="binomial",data = creditcard)
summary(cards)

#predict
pred<-predict(cards,type = c("response"),creditcard)
pred

#confusion matrix
confusion<-table(pred>0.5,creditcard$card)
confusion
# Model Accuracy
#adding diagonal elements in the confusion matrix
Accuracy1<-sum(diag(confusion))/sum(confusion)
Accuracy1

#the accuracy of the model is 0.77

#ROC Curve

library(ROCR)
library(pROC)

rocpred<-prediction(pred,creditcard$card)
rocperf<-performance(rocpred,'tpr','fpr')
plot(rocperf,colorize=T,text.adj=c(-0.2,1.7))

#lets check AREA UNDER THE CURVE
auc<-auc(creditcard$card ~pred)
auc

# Hence the accuracy is 0.6218

#Not much of data is under the area of the curve.
