library("klaR")
library("caret")
library("rpart")
library("neuralnet")
library("neuralnet")
library ("e1071")

library("adabag")
library("ada")
library("gbm")
library("mlbench")
library("randomForest")
library("ipred")
library("class")

library("Metrics")


wine <- read.csv("/Users/saivamsi/Downloads/mxs146030_sxm154231 copy/data.csv", header=TRUE)
attach(wine)
value <- as.integer(1)
write("wine data set:", file="output.txt",append=TRUE)

for(i in 1:1)
{
  write(paste("Iteration:",i), file="output.txt",append=TRUE)
  
  samples <- floor(0.2 * nrow(wine))
  test <- sample(seq_len(nrow(wine)), size = samples)
  wine.train = wine[-test,]
  wine.test = wine[test,]
  Class2<-wine.test[,as.integer(value)]
 
  #Neural Net
  method <- "Neural Network"
  n <- names(wine.test)
  trainingData<-sapply(wine.test,as.numeric)
  testData<-sapply(wine.test,as.numeric)
  f <- as.formula(paste(colnames(trainingData)[as.integer(value)],"~", paste(n[!n %in% colnames(trainingData)[as.integer(value)]], collapse = " + ")))
  f1<-paste(c(n[!n %in% colnames(trainingData)[as.integer(value)]]))
  creditnet <- neuralnet(f ,trainingData, hidden = 4, threshold = 0.1)
  temp_test <- subset(testData, select = c(f1))
  creditnet.results <- compute(creditnet, temp_test)
  results <- data.frame(actual = testData[,as.integer(value)], prediction = creditnet.results$net.result)
  results$prediction <- round(results$prediction)
  accuracy<-(sum(results[,2]==testData[,as.integer(value)])/nrow(testData))*100
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""
  
  #SVM
  method = "SVM"
  svm.model= svm(as.factor(ACTION)~.,data = wine.train, cost =100, gamma=1000)
  svm.pred= predict(svm.model, wine.test)
  conf = table(svm.pred, true = wine.test$ACTION)
  # print(conf)
  accuracy <-(sum(diag(conf))/sum(conf))*100
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""
  
  #Naive Bayesian
  method="Naive Bayesian"
  model = naiveBayes(as.factor(ACTION) ~ ., data = wine.train)
  model
  prediction <- predict(model, wine.test)
  prediction
  tab <- table(prediction, wine.test$ACTION)           
  # print(tab)
  accuracy <- (sum(diag(tab))/sum(tab))*100
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""

  #Random Forest
  method = "Random Forest"
  # cl<-train_data[,index]
  set.seed(300)
  rf <- randomForest(as.factor(ACTION) ~ .,data=wine.train,importance=TRUE, ntree=2000)
  predicted<-predict(rf,wine.test)
  tab <- table(predicted,wine.test$ACTION)
  # print(tab)
  accuracy<- (sum(predicted==wine.test$ACTION))/length(wine.test$ACTION)*100.0
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""


  #Bagging
  method = "Bagging"
  form <- as.formula(paste("as.factor(ACTION) ~","." ))
  bag <- ipred::bagging(form, data=wine.train, boos = TRUE,mfinal=10,
                          control = rpart.control(cp = 0)) 
  predicted<-predict(bag,wine.test)
  tab <- table(predicted,wine.test$ACTION)
  # print(tab)
  accuracy<- (sum(predicted==wine.test$ACTION))/length(wine.test$ACTION)*100.0
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""

  #Boosting
  method = "Boosting"
  n <- names(wine.train)
  form <- as.formula(paste("as.factor(ACTION) ~","." ))
  adaboost <- ada(form, data = wine.train, iter=20, nu=1, type="discrete")
  predicted<-predict(adaboost,wine.test)
  tab <- table(predicted,wine.test$ACTION)
  # print(tab)
  accuracy<- (sum(predicted==wine.test$ACTION))/length(wine.test$ACTION)*100.0
  met<-paste("Method:",method)
  acc<-paste("Accuracy:",accuracy)
  res<-paste(met, acc)
  write(res, file="output.txt",append=TRUE)
  met<-""
  acc<-""

}