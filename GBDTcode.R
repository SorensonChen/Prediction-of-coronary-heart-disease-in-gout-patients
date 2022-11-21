library(ROSE)
library(pROC)
library(gbm)

#saved randomly selected train_ind
train_ind<-read.csv("D:\\github\\train_ind.csv")
#Data set after data pre-processing
dataset <- read.csv("D:\\github\\dataset.csv")

dataset$sex<-as.factor(dataset$sex)
dataset$hypertension<-as.factor(dataset$hypertension)
dataset$smoke<-as.factor(dataset$smoke)
dataset$di<-as.factor(dataset$di)
dataset$nsaid<-as.factor(dataset$nsaid)


train<-dataset[as.numeric(train_ind),]
#combined sampling
train_over<-ovun.sample(y~.,data=train,p=0.5,method="both",seed = 1)
test<-dataset[as.numeric(-train_ind),]

# train model
model <- gbm(y~.,data=train_over$data,shrinkage=0.008,
             distribution='bernoulli',n.trees=60,verbose=F)

#predict
pred<-predict(model,test[,-9],ntrees=60,type='response')
#confusion matrix
table(ifelse(pred>0.5,TRUE,FALSE),test$y)
#ROC
gbdt_roc<-roc(test$y,pred,direction='<')
#AUC
auc(gbdt_roc)
