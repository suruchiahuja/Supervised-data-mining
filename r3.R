
#########################################
###Question 1 
###
#########################################

#install.packages("caret")
library(base)
library(MASS)
library(stats)
library(lattice)
library(ggplot2)
library(caret)
library(class)
library(ISLR)

attach(Auto)
mpg01 = rep(0, length(mpg)) #Creating the binary variable mpg01 containing all 0's with the length of mpg
mpg01[mpg > median(mpg)] = 1 #Contains 1 if value is above median of mpg 
Auto = data.frame(Auto, mpg01) #Using the dataframe function to create a single data set

######## Exploring the data graphically

new.data <- Auto[,c(2,3,4,5,6,7,8,10)]
plot(Auto)

#######Dividing the data into test and training set

set.seed(12345)
train <- sample(1:nrow(new.data),nrow(new.data)*0.53)
auto_train <- new.data[train,]
auto_test <- new.data[-train,]
mpg01.test = mpg01[-train]
dim(auto_train)
dim(auto_test)

y_true_train <- as.numeric(auto_train$mpg01)
y_true_test <- as.numeric(auto_test$mpg01)

##################################
#Linear Discriminant Analysis
###################################

lda.fit<-lda(mpg01~cylinders+displacement+horsepower+weight,data=auto_train)
lda.fit
lda.predict.train <- predict(lda.fit,data=auto_train)
y_hat_train <- as.numeric(lda.predict.train$class)-1
lda.predict.test <- predict(lda.fit,newdata=auto_test)
y_hat_test<-as.numeric(lda.predict.test$class)-1

### Computing the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train)
lda_test_error <- sum(abs(y_true_test-y_hat_test))/length(y_true_test)
lda_train_error
lda_test_error


################################## 
#Quadratic Discriminant Analysis
##################################

qda.fit<-qda(mpg01~cylinders+displacement+horsepower+weight,data=auto_train)
qda.fit
qda.predict.train <- predict(qda.fit,data=auto_train)
y_hat_train_qda <- as.numeric(qda.predict.train$class)-1
qda.predict.test <- predict(qda.fit,newdata=auto_test)
y_hat_test_qda<-as.numeric(qda.predict.test$class)-1

### Computing the error

qda_train_error <- sum(abs(y_true_train - y_hat_train_qda))/length(y_true_train)
qda_test_error <- sum(abs(y_true_test-y_hat_test_qda))/length(y_true_test)
qda_train_error
qda_test_error


##########################
#Logistic Regression
#########################

glm.fit <- glm(mpg01~cylinders+displacement+horsepower+weight,data=auto_train,family="binomial")
glm.fit
glm.predict.train <- predict(glm.fit,newdata=auto_train,type="response")
y_hat_train_lr <- round(glm.predict.train)
glm.predict.test <- predict(glm.fit,newdata=auto_test,type="response")
y_hat_test_lr <- round(glm.predict.test)

### Computing the error

train_error_lr <- sum(abs(y_true_train - y_hat_train_lr))/length(y_true_train)
test_error_lr <- sum(abs(y_true_test-y_hat_test_lr))/length(y_true_test)
train_error_lr
test_error_lr


#############################
#knn
#############################
knn.pred1 <- knn(auto_train,auto_test,auto_train$mpg01,k=10)
knn.pred1
mean(knn.pred1 != mpg01.test)

knn.pred2 <- knn(auto_train,auto_test,auto_train$mpg01,k=100)
knn.pred2
mean(knn.pred2 != mpg01.test)


########################################
###question 2
###
##########################################

library(klaR)  # install.packages("klaR")
library(MASS)  # install.packages("MASS")
library(mclust)

DiabetesAndrews36_1 <- read.table("C:\\Users\\SURUCH~1\\AppData\\Local\\Temp\\Rtmp0gdLLt\\data3fc36575beb", quote="\"", comment.char="")
#View(DiabetesAndrews36_1)
diabetes <- DiabetesAndrews36_1
diabetes1 <- diabetes[ , c(5,6,7,8,9)]
diabetes2 <- diabetes[ , c(5,6,7,8,9,10)]
class1 <- diabetes[ , c(10)]

##### Scatterplots
clPairs(diabetes1 , classification =class1 , symbols=as.character(1:3))

########### Diving into test set and training set

set.seed(12345)
train = sample(1:nrow(diabetes2), nrow(diabetes2)*.66)
diabetes_train = diabetes2[train, ]
diabetes_test = diabetes2[-train, ]
dim(diabetes_train)
dim(diabetes_test)

y_true_train <- as.numeric(diabetes_train$V10)
y_true_test <- as.numeric(diabetes_test$V10)


###############################
#LDA
###############################

lda.fit <- lda(V10~V5+V6+V7+V8+V9, data = diabetes_train)
lda.fit
lda.predict.train <- predict(lda.fit, newdata = diabetes_train)
y_hat_train <- as.numeric(lda.predict.train$class)
lda.predict.test <- predict(lda.fit, newdata = diabetes_test)
y_hat_test <- as.numeric(lda.predict.test$class)

### Compute the error
lda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) # 0.22
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)  # 0.17
lda_train_error
lda_test_error

####################################
#   Quadratic Discriminant Analysis
####################################
qda.fit <- qda(V10~V5+V6+V7+V8+V9, data = diabetes_train)
qda.fit
qda.predict.train = predict(qda.fit, newdata = diabetes_train)
y_hat_train <- as.numeric(qda.predict.train$class)
qda.predict.test = predict(qda.fit, newdata = diabetes_test)
y_hat_test <- as.numeric(qda.predict.test$class)

### Compute the error
qda_train_error <- sum(abs(y_true_train - y_hat_train))/length(y_true_train) 
qda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)
qda_train_error
qda_test_error


###############################################
### Question 4
##
###############################################

library(boot)
library(glmnet)
set.seed(1)
x=rnorm(100)
y=x-2*x^2+rnorm(100)

dat=data.frame(x=x,y=y)
fun<-function(i){ 
  glmfiti=glm(y~poly(x,i),data=dat) #Performing linear regression
  cv.err=cv.glm(dat,glmfiti) #part of the boot library
  cv.err$delta[1] 
}
loocverror = lapply(seq(4),fun)
loocverror
glmfiti=glm(y~poly(x,4),data=dat)
summary(glmfiti)

##############################################







