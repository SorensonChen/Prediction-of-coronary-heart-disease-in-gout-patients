library(ROSE)
library(randomForest)
library(pROC)

#saved randomly selected train_ind
train_ind<-read.csv("D:\\github\\train_ind.csv")
#Data set after data pre-processing
dataset <- read.csv("D:\\github\\dataset.csv")


dataset$sex<-as.factor(dataset$sex)
dataset$hypertension<-as.factor(dataset$hypertension)
dataset$smoke<-as.factor(dataset$smoke)
dataset$di<-as.factor(dataset$di)
dataset$nsaid<-as.factor(dataset$nsaid)
dataset$y <- as.factor(dataset$y)

train<-dataset[as.numeric(train_ind),]
#combined sampling
train_over<-ovun.sample(y~.,data=train,p=0.5,method="both",seed = 1)
test<-dataset[as.numeric(-train_ind),]

#train model
set.seed(1)
modelrf <- randomForest(y~.,data = train_over$data,ntree =65,mtry=7,importance=TRUE,proximity=TRUE,type=classification)
#feature importance
importance<-modelrf$importance
#prediction
xx <- subset(test,select=-y)
#Prediction Label
pre_ran <- predict(modelrf,newdata=xx)
#Prediction Probability
pre_randd <- predict(modelrf,newdata=xx,"prob")
tt<-roc(test$y,pre_randd[,"1"],direction='<')
as.numeric(auc(tt))

#confusion matrix
table(test$y,pre_ran,dnn=c("True Classes","Predicted Classes"))

