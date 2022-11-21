library(e1071)
library(MASS)
library(ROSE)
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

#trainmodel
svm_model = svm(y~.,data=train_over$data,knernel = "radial",probability=TRUE,gamma=0.1,cost=1)
summary(svm_model)
#predict
svm_pred=predict(svm_model,test,probability = TRUE)
table(test$y,svm_pred)

#ROC and AUC
roc_svm <- roc(response = test$y, predictor =attr(svm_pred, "probabilities")[,"1"],direction = "<")
auc(roc_svm)
