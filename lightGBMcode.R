library(lightgbm)
library(ROSE)
library(Matrix)
library(pROC)

#saved randomly selected train_ind
train_ind <- read.csv("D:\\github\\train_ind.csv")
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

#Set parameters for model training
train_params <- list(
  num_leaves = 7L
  , learning_rate = 1.0
  , objective = "binary"
)

#Convert training set to lgb.Dataset
trainx<-data.matrix(train_over$data[,-9])
trainy<-train_over$data[,9]
dtrain<-lightgbm::lgb.Dataset(data=trainx,label=trainy,colnames=c("sex","age","BMI","smoke","hypertension","cr","di","nsaid"))

#train model
lightgbm_model<-lgb.train(data=dtrain,
                          params=train_params,
                          nrounds=2L
)

#Transformation of test set independent variables into matrices
testdata1 <- data.matrix(test[,c(1:8)])

#prediction
pred <- predict(lightgbm_model, testdata1)
#confusion matrix
table(ifelse(pred>0.5,TRUE,FALSE),test$y)
#ROC and AUC
lightgbmROC <- roc(test$y,pred,direction='<')
auc(lightgbmROC)

