#######################
###### Question 3
#########################

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
set.seed(12345)
test_indis <- sample(1:nrow(my_dataset), .20*nrow(my_dataset))
test <- my_dataset[test_indis, ]
training <- my_dataset[-test_indis, ]
y_true <- as.numeric(test$High)-1

####Random forest
rf.fit <- randomForest(High~., data = training, n.tree= 10000)
x11()
varImpPlot(rf.fit)
importance(rf.fit)
y_hat <- predict(rf.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_rf <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_rf #0.0198

##########Bagging
bag.fit <- randomForest(High~., data = training, n.tree= 10000, mtry = 13)
x11()
varImpPlot(bag.fit)
importance(bag.fit)
y_hat <- predict(bag.fit, newdata = test, type = "response")
y_hat <- as.numeric(y_hat)-1
misclass_bag <- sum(abs(y_true- y_hat))/length(y_hat)
misclass_bag #0.0297

######Boosting
boost.train <- training;
boost.train$High <- as.numeric(training$High)-1
boost.test <- test;
boost.test$High <- as.numeric(test$High)-1

boost.fit <- gbm(medv~.,data = boost.train, n.trees = 5000,shrinkage = 0.6,interaction.depth = 4,distribution = "gaussian")

names(boost.fit)
summary(boost.fit)

y_hat <- predict(boost.fit,newdata = boost.test,n.trees = 5000,type = "response")
misclass_boost <- sum(abs(y_hat - y_true))/length(y_true)
misclass_boost 

################ Question 7

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
svm.model <- tune(svm, Purchase~., data = train,kernel="linear", ranges = list(cost = c(0.01,0.05,0.1,1,3,5,7,9,10)))
svm.model
summary(svm.model)

bestmod <- svm.model$best.model
bestmod


y_hat_test <- predict(bestmod, newdata = test)
y_true <- test$Purchase
table(y_true,y_hat_test)

y_hat_train <- predict(bestmod, newdata = train)
y_true <- train$Purchase
table(y_true,y_hat_train)

############# Part b######
####Svm with a radial kernel
##########################

svm.radial <- tune(svm, Purchase~., data = train, kernel = "radial",ranges = list(cost = c(0.01,0.05,0.1,1,3,5,7,9,10)))
svm.radial
summary(svm.radial)

bestmod1 <- svm.radial$best.model
bestmod1


y_hat_test <- predict(bestmod1, newdata = test)
y_true <- test$Purchase
table(y_true,y_hat_test)

y_hat_train <- predict(bestmod1, newdata = train)
y_true <- train$Purchase
table(y_true,y_hat_train)

#### kernel of degree = 2

svm.poly <- tune(svm, Purchase~., data = train, kernel = "poly",degree = 2,ranges = list(cost = c(0.01,0.05,0.1,1,3,5,7,9,10)))
svm.poly
summary(svm.poly)

bestmod1 <- svm.poly$best.model
bestmod1

y_hat_test <- predict(bestmod1, newdata = test)
y_true <- test$Purchase
table(y_true,y_hat_test)

y_hat_train <- predict(bestmod1, newdata = train)
y_true <- train$Purchase
table(y_true,y_hat_train)
##########################################




################################################
############### Question 1
#################################################
library(ElemStatLearn)
library(bootstrap)
library(boot)
library(leaps)

data(prostate)
prostate <- prostate

set.seed(123)
train = sample(1:nrow(prostate), .80*nrow(prostate))
Y.train = prostate$age[train]
Y.test = prostate$age[-train]
X.train = prostate[train, ]
X.test = prostate[-train, ]

fit <- lm(age ~., data = prostate[train,])
pred.test <- predict(fit, newdata = X.test)
pred.train <- predict(fit, newdata = X.train)

test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
test.error
train.error

########
set.seed(123)
train = sample(1:nrow(prostate), .80*nrow(prostate))
Y.train = prostate$age[train]
Y.test = prostate$age[-train]

training = prostate[train,1:9 ]
testing = prostate[-train, 1:9]

fit <- regsubsets(age~., data = training, method = "exhaustive", nvmax = 8)
my_summary <- summary(fit)
names(my_summary)
my_summary$cp
my_summary$bic

which.min(my_summary$cp) #Cp says 5 variables is best
which.min(my_summary$bic) #BIC says 5 variables is best

#selection based on hold out method
select = summary(fit)$outmat
train.error.store <- c()
test.error.store <- c()
for (i in 1:9){
  temp <- which(select[i,] == "*")
  temp <- temp + 2
  
  red.training <- training[, c(1,temp)]
  red.testing <- testing[,c(1,temp)]
  
  red.fit <- lm(age~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training)
  pred.test = predict(red.fit, newdata = red.testing)
  
  test.error <- (1/length(Y.test))*sum((pred.test - Y.test)^2)
  train.error <- (1/length(Y.train))*sum((pred.train - Y.train)^2)
  
  train.error.store <- c(train.error.store, train.error)
  test.error.store <- c(test.error.store, test.error)
  
}






