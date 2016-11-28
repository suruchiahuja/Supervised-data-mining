###########################################
######### Question 2
##########################################
library("HDclassif")
library("rpart")
library("tree")
library("caret")
require(ggplot2)

####attaching the data
data(wine)
attach(wine)
wine_data<-wine[,-c(2,8,9)]
colnames(wine_data)<-c("Class","MalicAcid","Ash","AlcAsh","Mg","Phenols","Proa","Color","Hue","OD","Proline")

set.seed(1)
train <- sample(1:nrow(wine_data), .80*nrow(wine))
train_set= wine_data[train,]
test_set= wine_data[-train,]
y_true<- as.factor(test_set$class)
y_true

model.control <- rpart.control(minsplit = 3, xval = 10, cp = 0)
fit.wine <- rpart(as.factor(Class)~., data = train_set, method = "class", control = model.control)
X11()
plot(fit.wine,branch=0.3,uniform=T,compress = T,main="Full tree")
text(fit.wine,use.n=T,all=T,cex=0.5)
fit.wine

#pruning a tree

min_cp = which.min(fit.wine$cptable[,4])
pruned_fit_wine <- prune(fit.wine, cp = fit.wine$cptable[min_cp,1])
X11()
plot(pruned_fit_wine,branch=0.3,uniform=T,compress = T,main="Pruned tree")
text(pruned_fit_wine,use.n=T,all=T,cex=0.5)

pred_train <-predict(pruned_fit_wine, newdata = train_set, type = "class")
pred_test <- predict(pruned_fit_wine, newdata = test_set, type = "class")
compares <- data.frame(pred_test,test_set$class)

####################################################
###### Question 3
####################################################

#install.packages("rpart")
#install.packages("randomForest")
#install.packages("ggplot2")
#install.packages("gbm")
#install.packages("caret")
library(rpart)
library(randomForest)
library(gbm)
library(ggplot2)
library(caret)
library(ISLR)
library(MASS)

#Using the dataset Boston
data(Boston)
dataset <- Boston
names(dataset)
High <- ifelse(Boston$crim <= 4,"NO","YES")
my_dataset <- data.frame(dataset[,-1],High)
head(my_dataset)

#Creating a test set and a training set
set.seed(123)
test_indis <- sample(1:nrow(my_dataset), .20*nrow(my_dataset))
test <- my_dataset[test_indis, ]
training <- my_dataset[-test_indis, ]
y_true <- as.numeric(test$High)-1

y_true_train <- as.numeric(training$High)-1
y_true_test <- as.numeric(test$High)-1


###########Logistic Regression
glm.fit <- glm(High ~., data = training, family = "binomial")
summary(glm.fit)
glm.probs.train <- predict(glm.fit, newdata = training, type = "response")
y_hat_train <- round(glm.probs.train)
glm.probs.test <- predict(glm.fit, newdata = test, type = "response")
y_hat_test <- round(glm.probs.test)
train_err <- sum(abs(y_hat_train- y_true_train))/length(y_true_train)
test_err <- sum(abs(y_hat_test- y_true_test))/length(y_true_test)

train_err
test_err

####Random forest
rf.fit <- randomForest(High~., data = training, n.tree= 10000)
x11()
varImpPlot(rf.fit)
importance(rf.fit)
y_hat <- predict(rf.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf 

##########Bagging
bag.fit <- randomForest(High~., data = training, n.tree= 10000, mtry = 13)
x11()
varImpPlot(bag.fit)
importance(bag.fit)
y_hat <- predict(bag.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_bag <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_bag 

######Boosting
boost.train <- training;
boost.train$High <- as.numeric(training$High)-1
boost.test <- test;
boost.test$High <- as.numeric(test$High)-1

boost.fit <- gbm(High~.,data = boost.train,n.trees = 1000,shrinkage = 0.1,interaction.depth = 3,distribution = "adaboost")
boost.fit2 <- gbm(High~.,data = boost.train,n.trees = 1000,shrinkage = 0.6,interaction.depth = 3,distribution = "adaboost")

names(boost.fit)
summary(boost.fit)
names(boost.fit2)
summary(boost.fit2)

y_hat <- predict(boost.fit,newdata = boost.test,n.trees = 1000,type = "response")
y_hat2 <- predict(boost.fit2,newdata = boost.test,n.trees = 1000,type = "response")
misclass_boost <- sum(abs(y_hat - y_true))/length(y_true)
misclass_boost 
misclass_boost2 <- sum(abs(y_hat2 - y_true))/length(y_true)
misclass_boost2 


#################################################################
############ Question 4
#################################################################
library("rpart")
library("gbm")
library("randomForest")
library("geneplotter")
library("ElemStatLearn")

data(spam)
attach(spam)
spam_data<-spam
spam_data$spam <- as.numeric(spam_data$spam)-1
spam_data$spam

set.seed(1234)
test_indis<-sample(1:nrow(spam_data),0.20*nrow(spam_data))
spam_test<-spam_data[test_indis,]
spam_train<-spam_data[-test_indis,]
y_true<-spam_data$spam

##m=1
rf1<-randomForest(as.factor(spam) ~.,data=spam_train,n.tree=1000,mtry=1,keep.inbag=T,oob.prox=T,importance=T,main="random forest for m=10")
X11()
varImpPlot(rf1)
importance(rf1)
rf1$oob.times
print(rf1)

y_hat1<-predict(rf1,newdata=spam_test,type="response")
y_hat1<-as.numeric(y_hat1)-1
misclass1<-sum(abs(spam_test$spam-y_hat1))/length(y_hat1)
misclass1 

##m=3
rf2<-randomForest(as.factor(spam) ~.,data=spam_train,n.tree=1000,mtry=3,keep.inbag=T,oob.prox=T,importance=T)
X11()
varImpPlot(rf2)
importance(rf2)
rf2$oob.times
print(rf2)

y_hat2<-predict(rf2,newdata=spam_test,type="response")
y_hat2<-as.numeric(y_hat2)-1
misclass2<-sum(abs(spam_test$spam-y_hat2))/length(y_hat2)
misclass2

##m=5
rf3<-randomForest(as.factor(spam) ~.,data=spam_train,n.tree=1000,mtry=5,keep.inbag=T,oob.prox=T,importance=T)
X11()
varImpPlot(rf3)
importance(rf3)
rf3$oob.times
print(rf3)

y_hat3<-predict(rf3,newdata=spam_test,type="response")
y_hat3<-as.numeric(y_hat3)-1
misclass3<-sum(abs(spam_test$spam-y_hat3))/length(y_hat3)
misclass3

##m=8
rf4<-randomForest(as.factor(spam) ~.,data=spam_train,n.tree=1000,mtry=8,keep.inbag=T,oob.prox=T,importance=T)
X11()
varImpPlot(rf4)
importance(rf4)
rf4$oob.times
print(rf4)

y_hat4<-predict(rf4,newdata=spam_test,type="response")
y_hat4<-as.numeric(y_hat4)-1
misclass4<-sum(abs(spam_test$spam-y_hat4))/length(y_hat4)
misclass4

mtr<-c(1,3,5,8)
oob<-c(0.0769,0.0541,0.0508,0.0481)
miscl<-c(0.0706,0.0467,0.04239,0.0456)
x11()
plot(oob,mtr,xlim = c(0.048,0.0600))
lines(oob,mtr,col="blue")
lines(miscl,mtr)

################################################################
########## Question 5
###############################################################

library(neuralnet)
library(ElemStatLearn)

spam_data<- read.table(file.choose(),header = F,sep = ",")
set.seed(1234)
train = sample(1:nrow(spam_data), .80*nrow(spam_data))
Y.train = spam_data$V58[train]
Y.test = spam_data$V58[-train]

training = spam_data[train, ]
testing = spam_data[-train, ]

n<-colnames(spam_data[,-58])
n1<-paste(n,collapse="+")
formula <- paste("V58~",n1)
nn <- neuralnet(as.formula(formula), data = training, hidden = 1, err.fct = 'ce', linear.output = FALSE)

names(nn)
plot(nn)
nn$result.matrix

#train error
true_class = (training$V58)
pred_class = round(nn$net.result[[1]])
error_train = sum(abs(true_class - pred_class))/length(pred_class) #0.0467391
error_train

#test error
true_class1 = (testing$V58)
pred_class = round(nn$net.result[[1]])
error_test = sum(abs(true_class1 - pred_class))/length(pred_class) #0.4711956
error_test

new.output <- compute(nn, covariate = testing[ ,1:57])
summary(new.output)


#############################################################################
############### Question 7
#########################################################################

#install.packages("e1071")
library(ISLR)
library(e1071)

data(OJ)
data <- OJ

set.seed(123)
train_indis <- sample(1:nrow(data), 0.8*nrow(data))
train <- data[train_indis, ]
test <- data[-train_indis, ]

### SVM in a linear model
train_error = vector()
test_error = vector()
costs <- c(0.01, 0.05, 0.1, 0.15, 0.2, 1, 2, 3.5, 4, 8.5, 10)

for(i in 1:length(costs)) {
  tune.model <- tune(svm, Purchase~., data = train, kernel = "linear",
                     ranges = list(cost = costs[i]))
  bestmod <- tune.model$best.model
  
  y_hat <- predict(bestmod, newdata = train)
  y_true <- train$Purchase
  train_error[i] <- length(which(y_hat == y_true))/length(y_true)
  
  # predict the test data
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_error[i] <- length(which(y_hat == y_true))/length(y_true)
}

x = c(1:length(train_error), 1:length(train_error))
x11()
plot(x, c(train_error, test_error), pch=" ",ylab="error rate", xaxt = "n", xlab="cost")
points(train_error, col="red")
points(test_error, col="blue")
axis(1, at=1:length(train_error), labels=costs)

############# Part b######
####Svm with a radial kernel
##########################

train_error = vector()
test_error = vector()

for(i in 1:length(costs)) {
  tune.model <- tune(svm, Purchase~., data = train, kernel = "radial",
                     ranges = list(cost = costs[i]))
  bestmod <- tune.model$best.model
  
  y_hat <- predict(bestmod, newdata = train)
  y_true <- train$Purchase
  train_error[i] <- length(which(y_hat == y_true))/length(y_true)
  
  # predict the test data
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_error[i] <- length(which(y_hat == y_true))/length(y_true)
}

x = c(1:length(train_error), 1:length(train_error))
x11()
plot(x, c(train_error, test_error), pch=" ", ylab="error rate", xaxt = "n", xlab="cost")
points(train_error, col="red")
points(test_error, col="blue")
axis(1, at=1:length(train_error), labels=costs)


#### kernel of degree = 2

train_error = vector()
test_error = vector()

for(i in 1:length(costs)) {
  tune.model <- tune(svm, Purchase~., data = train, kernel = "polynomial",
                     ranges = list(cost = costs[i]), degree = 2)
  bestmod <- tune.model$best.model
  
  y_hat <- predict(bestmod, newdata = train)
  y_true <- train$Purchase
  train_error[i] <- length(which(y_hat == y_true))/length(y_true)
  
  # predict the test data
  y_hat <- predict(bestmod, newdata = test)
  y_true <- test$Purchase
  test_error[i] <- length(which(y_hat == y_true))/length(y_true)
}

x = c(1:length(train_error), 1:length(train_error))
x11()
plot(x, c(train_error, test_error), pch=" ", ylab="error rate", xaxt = "n", xlab="cost")
points(train_error, col="red")
points(test_error, col="blue")
axis(1, at=1:length(train_error), labels=costs)



############################################################
###########################################################
############################################################################################









