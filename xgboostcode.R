library(xgboost)
library(Matrix)
library(pROC)
library(ROSE)

#saved randomly selected train_ind
train_ind<-read.csv("D:\\github\\train_ind.csv")
#Data set after data pre-processing
dataset <- read.csv("D:\\github\\dataset.csv")

train<-dataset[as.numeric(train_ind),]
#combined sampling
train_over<-ovun.sample(y~.,data=train,p=0.5,method="both",seed = 1)
test<-dataset[as.numeric(-train_ind),]

#The xgb.DMatrix object needed to construct
traindata1 <- data.matrix(train_over$data[,c(1:8)]) 
traindata2 <- Matrix(traindata1,sparse=T) 
traindata3 <- train_over$data[,9]
traindata4 <- list(data=traindata2,label=traindata3)
dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 


#The xgb.DMatrix object needed to construct
testset1 <- data.matrix(test[,c(1:8)]) 
testset2 <- Matrix(testset1,sparse=T) 
testset3 <- test[,9]
testset4 <- list(data=testset2,label=testset3) 
dtest <- xgb.DMatrix(data = testset4$data, label = testset4$label)

#train model
set.seed(12)
xgb <- xgb.train(data = dtrain,max_depth=50, eta=0.25,objective='binary:logistic', nround=17,eval_metric = "auc")

#predict
pre_xgb = predict(xgb,newdata = dtest)

#confusion matrix
table(pre_xgb>0.5,test$y)
#ROC and AUC
xgboost_roc <- roc(test$y,pre_xgb,direction='<')
as.numeric(auc(xgboost_roc))
