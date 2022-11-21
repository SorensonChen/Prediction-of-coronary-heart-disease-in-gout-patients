library(ROSE)
library(pROC)

#saved randomly selected train_ind
train_ind<-read.csv("D:\\github\\train_ind.csv")
#Data set after data pre-processing
dataset <- read.csv("D:\\github\\dataset.csv")

train<-dataset[as.numeric(train_ind),]
#combined sampling
train_over<-ovun.sample(y~.,data=train,p=0.5,method="both",seed = 1)
test<-dataset[as.numeric(-train_ind),]
  
#train model
model <- glm(y~.,family=binomial,data=train_over$data)
#stepwise
step_model<-step(model,direction='both')

#test model
xx<-subset(test,select=-y)
k<-test[,"y"]
b<-summary(step_model)
pred1<-predict(step_model,xx,type="response")

#confusion matrix
table(pred1>0.5,k)

#ROC and AUC
oo<-roc(k,pred1,direction='<')
as.numeric(auc(oo))

