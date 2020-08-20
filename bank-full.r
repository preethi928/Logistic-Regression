deposit <- read.csv("C:/Data_science/EXCLER/My Assignments/Logistic regression/bank-full.csv", sep=";")
View(deposit)

#EDA 

is.na(deposit)
sum(is.na(deposit))

#NA VALUES ARE 0
#GLM(generalized Linear Model)
str(deposit)
colnames(deposit)
#df<-deposit[1:120,]
#View(df)
logit<-glm(y ~ default+housing+loan,family ="binomial",data = deposit)
summary(logit)

#confusion Matr
print(as.factor(deposit$age))
length(levels(as.factor(deposit$age)))

#prediction
pred<-predict(logit,type = c("response"),deposit)
View(pred)

#confusion matrix
confusion<-table(deposit$y)
confusion
# Model Accuracy
#adding diagonal elements in the confusion matrix
Accuracy<-sum(diag(confusion))/sum(confusion)
Accuracy
