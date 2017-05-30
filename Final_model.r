train<-read.csv('F:/dataquest dataset/traindata_R.csv')
test<-read.csv('F:/dataquest dataset/testdata_R.csv')
# The exploratory analysis has been done in python
# No missing values
# Does not recquire Outlier Treatment 
# The answers are present in the python model
str(train)
summary(train$Wife_age)
prop.table(table(train$Party_voted_for))
prop.table(table(train$Wife_education))
table(train$Number_of_children_ever_born)
hist(train$Wife_age)
library(gmodels)
CrossTable(train$Wife_education,train$Party_voted_for)
library(ggplot2)
set.seed(333)
#logistic regression model
model <- glm (as.factor(Party_voted_for) ~ ., data = train, family = binomial)
summary(model)
predict <- predict(model,type='response')
#confusion matrix
table(train$Party_voted_for, predict > 0.5)
#ROCR Curve

library(gplots)
library(ROCR)

ROCRpred <- prediction(predict, train$Party_voted_for)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(ROCRpred,"auc")
auc
#DecisionTree
library(rpart)
set.seed(333)
train.tree<-rpart(Party_voted_for ~ .,data=train,method='class',control=rpart.control(maxdepth = 14,minsplit = 15,minbucket = 50))
summary(train.tree)
library(rpart.plot)
rpart.plot(train.tree)

prediction_test<-predict(train.tree,newdata = test,type='class')
predict<-predict(train.tree,newdata = test,type='prob')
predict[1:491]
library(caret)
confusionMatrix(prediction_test,test$Party_voted_for)
# ROC
ROCRpred <- prediction(1-predict[1:491], test$Party_voted_for)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(ROCRpred,"auc")
auc
# RandomForest
library(randomForest)
train.tree<-randomForest(as.factor(Party_voted_for) ~ .,data=train,importance=TRUE,nTree=1000)
plot(train.tree)
prediction_test<-predict(train.tree,newdata = test)
predict<-predict(train.tree,newdata = test,type='prob')
prediction_test
confusionMatrix(prediction_test,test$Party_voted_for)

# ROC
ROCRpred <- prediction(1-predict[1:491], test$Party_voted_for)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(ROCRpred,"auc")
auc
