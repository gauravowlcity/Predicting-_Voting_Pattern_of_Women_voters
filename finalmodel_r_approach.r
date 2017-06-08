library(dplyr)
library(ggplot2)
library(gmodels)
library(gplots)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)

train<-read.csv('F:/dataquest dataset/traindata_R.csv')
test<-read.csv('F:/dataquest dataset/testdata_R.csv')
head(train)

# dimensions of dataset
dim(train)
dim(test)
# names of columns in the dataset
names(train)
# Missing Values
sapply(train,function(x) sum(is.na(x)))
sapply(test,function(x) sum(is.na(x)))
# Finding Unique Values in dataset
sapply(train, function(x) length(unique(x)))

# Exploratory Data Analysis
glimpse(train)

# Lets try to see if we could find some patterns 
ggplot(train, aes(x =Wife_age , fill = as.factor(Party_voted_for))) +
  geom_density(alpha = .3)
ggplot(train, aes(x =Wife_age )) +
  facet_grid(~Party_voted_for)+
  geom_histogram(bins=7)

ggplot(train, aes(x =Husband_education )) +
  facet_grid(~Party_voted_for)+
  geom_histogram()

ggplot(train, aes(x =Wife_education )) +
  facet_grid(~Party_voted_for)+
  geom_histogram()

ggplot(train, aes(x =Media_exposure )) +
  facet_grid(~Party_voted_for)+
  geom_histogram()

ggplot(train, aes(x =Wife_education )) +
  facet_grid(~Party_voted_for~Number_of_children_ever_born)+
  geom_histogram(bins=7)

ggplot(train, aes(x = Wife_age)) + 
   geom_density()+
  facet_grid(~Party_voted_for)

ggplot(train,aes(x=as.factor(Party_voted_for),y=Wife_age))+
  geom_boxplot()
table(train$Wife_education,train$Party_voted_for)
prop.table(table(train$Wife_education,train$Party_voted_for))
select(train,Wife_age,Party_voted_for)
str(train)
summary(train$Wife_age)
prop.table(table(train$Party_voted_for))
prop.table(table(train$Wife_education))
prop.table(table(train$Husband_education))
table(train$Number_of_children_ever_born,train$Party_voted_for)
table(test$Number_of_children_ever_born,test$Party_voted_for)
prop.table(table(train$Number_of_children_ever_born))
plot(prop.table(table(train$Number_of_children_ever_born)))
plot(prop.table(table(test$Number_of_children_ever_born)))
hist(train$Wife_age)

CrossTable(train$Wife_education,train$Party_voted_for)


# set a seed value to have same results
set.seed(333)
#logistic regression model
model <- glm (as.factor(Party_voted_for) ~ ., data = train, family = binomial(link='logit'))
summary(model)
predict <- predict(model,newdata=test,type='response')
predict_test<-ifelse(predict>=0.5,1,0)
predict_test
#confusion matrix
confusionMatrix(prediction_test,test$Party_voted_for)
table(test$Party_voted_for, predict >=0.5)
#ROCR Curve

ROCRpred <- prediction(predict, test$Party_voted_for)
ROCRperf <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.7))
title('Logistic Regression Roc Curve')
auc <- performance(ROCRpred,"auc")
auc@y.values[[1]]
print(paste("The AUC score of the LogisticRegression model is:",auc@y.values[[1]]))


#DecisionTree
set.seed(333)
train.tree<-rpart(Party_voted_for ~ .,data=train,method='class',control=rpart.control(maxdepth = 14,minsplit = 12,minbucket = 50),parms = list(split = "information"))
#summary(train.tree)
rpart.plot(train.tree)

prediction_test<-predict(train.tree,newdata = test,type='class')
predict<-predict(train.tree,newdata = test,type='prob')
prob<-predict[,2]
confusionMatrix(prediction_test,test$Party_voted_for)
as.table(confusionMatrix(prediction_test,test$Party_voted_for),row.names())
conf_mat<-table(prediction_test,test$Party_voted_for)
conf_mat
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("The accuracy of the DecisionTreeModel is:",accuracy))

# Variable importance
varImp(train.tree)
# ROC
ROCRpred2 <- prediction(prob, test$Party_voted_for)
ROCRperf2 <- performance(ROCRpred2, 'tpr','fpr')
plot(ROCRperf2, colorize = TRUE, text.adj = c(-0.2,1.7))
auc <- performance(ROCRpred2,"auc")
print(paste("The AUC score of the DecisonTree model is:",auc@y.values[[1]]))


# RandomForest
train.tree<-randomForest(as.factor(Party_voted_for) ~ .,data=train,nTree=1000)
plot(train.tree)
plot(train.tree, type="simple")

# Variable importance curve
varImp(train.tree)

prediction_test<-predict(train.tree,newdata = test)
predict_proba<-predict(train.tree,newdata = test,type='prob')
prob<-predict_proba[,2]
confusionMatrix(prediction_test,test$Party_voted_for)

# ROC
ROCRpred3 <- prediction(prob, test$Party_voted_for)
ROCRperf3 <- performance(ROCRpred3, 'tpr','fpr')
plot(ROCRperf3, colorize = TRUE)
title("ROC Curve for RandomForest Model")
auc <- performance(ROCRpred3,"auc")
print(paste("The AUC score of the RandomForest model is:",auc@y.values[[1]]))
accuracy<-sum(diag(conf_mat))/sum(conf_mat)
print(paste("The accuracy of the RandomForestModel is:",accuracy))
############################################################
# Answers to assignment question

train%>%
  group_by(Wife_education)%>%
  summarise(count=n(),average_age=mean(Wife_age),
            average_children=mean(Number_of_children_ever_born),
            per_working=mean(Wife_working==1)*(100),
            per_high_standad=mean(Standard_of_living_index==4)*100
          )

#####################################################3
# Answer 3

plot(ROCRperf2, col='green')
plot(ROCRperf3,add=TRUE, col = 'red')
title('Multiples ROC Curves')
legend('bottomright', legend=c("ROC DecisionTree", "ROC RandomForest"),
       col=c("green", "red"), lty=1:2, cex=0.8)
