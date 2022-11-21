library(rpart)
library(rpart.plot)
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


#train DT model
model <- rpart(y~.,data = train_over$data,method = "class")
model1 <- prune(model,cp=0.046)

#test model
xx <- subset(test,select=-y)

#prediction label
pred <- predict(model1,xx,type="class")
#real label
k<-test[,"y"]

#prediction probability
pre_randd <- predict(model1,newdata=xx,"prob")
#ROC and AUC
gg<-roc(k,pre_randd[,"1"],direction='<')
as.numeric(auc(gg))
#confusion matrix
table(pred,k)

