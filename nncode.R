library(neuralnet)
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
set.seed(14)
network<-neuralnet(y~sex+age+BMI+smoke+hypertension+cr+di+nsaid,
                   data=train_over$data,act.fct = 'logistic',hidden = 40,
                   algorithm ="rprop-",linear.output = FALSE)


#predict
xx<-test[,-9]
pre <- predict(network,xx)
label<-test$y
table(pre>0.5,label) 

#ROC and AUC
nnet_roc <- roc(as.numeric(test$y),as.numeric(pre),direction = '<')
as.numeric(auc(nnet_roc))
