#############################################################
#### Question-2
#############################################################

install.packages("ISLR")
library("ISLR")
library("MASS")
library("leaps")
library("glmnet")
Caravan$Purchase <- as.numeric(Caravan$Purchase)

####ols-linear model
lm.model <- lm(Purchase~., data=Caravan)
plot(lm.model)

####forward subset selection
forward <- regsubsets(Purchase~., data=Caravan, nvmax=86, method = "forward")
summary(forward)$outmat[50, ]

####backward subset selection
backward <- regsubsets(Purchase~., data=Caravan, nvmax=86, method = "backward")
summary(backward)$outmat[50,]

####Ridge regression
X <- as.matrix(Caravan[,1:20])
Y <- as.matrix(Caravan$Purchase)
ridge.mod <- glmnet(X,Y,alpha = 0)
set.seed(12345)
cv.out <- cv.glmnet(X,Y,alpha=0)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
plot(ridge.mod)
ridge.pred <- predict(ridge.mod, s=bestlam,type="coefficients")

####Lasso Regression
lasso.mod <- glmnet(X,Y,alpha = 1)
cv_out <- cv.glmnet(X,Y, alpha=1)
bestlam1 <- cv_out$lambda.min
lasso_pred <- predict(lasso.mod,s=bestlam1,type= "coefficients")
plot(lasso.mod)





##############################################################
#### Question- 4
##############################################################

#install.packages("ISLR")
#install.packages("glmnet")
#install.packages("pls")
library(ISLR)
library(glmnet)
library(pls)

#loading collge data set
n <- floor(0.7 * nrow(College))
set.seed(123)
data(College)
College$Private <- as.numeric(College$Private)
train.size <- sample(seq_len(nrow(College)), size = n)
train <- (College[train.size, ])
X.train <- as.matrix(train[,c(1,3:18)])
Y.train <- as.matrix(train[,2])
test <- (College[-train.size, ])
X.test <- as.matrix(test[,c(1,3:18)])
Y.test <- as.matrix(test[,2])

#####fitting linear model
lm.fit <- lm(train$Apps ~., data=train) 
lm.predict <- predict(lm.fit, test)
mean((test[, "Apps"] - lm.predict)^2)

##### Ridge Regression Model
ridge.mod = glmnet(X.train, Y.train, alpha=0)
names(ridge.mod)
ridge.mod$lambda[10]
coef(ridge.mod)[,10]
l2 <- sqrt(coef(ridge.mod)[2:18,19]^2)
l2

###### Choosing the model by cross validation
set.seed(12345)
cv.out <- cv.glmnet(X.train, Y.train, alpha=0)
plot(cv.out)
names(cv.out)
bestlam <- cv.out$lambda.min
bestlam
rid_pred <- predict(ridge.mod, s=bestlam,type="coefficients")
rid_pred2 <- predict(ridge.mod, s=bestlam, newx=X.test,type="response")
y_hat <- rid_pred2
y_true <- Y.test
test_error <- sum((y_hat-y_true)^2)

#### Lasso Regression model
lasso.mod <- glmnet(X.train,Y.train,alpha = 1)
plot(lasso.mod)
cv.out <- cv.glmnet(X.train,Y.train, alpha=1)
plot(cv.out)
best.lam <- cv.out$lambda.min
lasso.pred <- predict(lasso.mod,s=best.lam,type="coefficients")
lasso.pred2 <- predict(lasso.mod,s=best.lam,newx=X.test,type="response")
y.hatl <- lasso.pred2
y.truel <- Y.test
test.error.lasso <- sum((y.hatl-y.truel)^2)
test.error.lasso


#####PCR
pcr.fit <- pcr(train$Apps~.,data=train,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")
pcr.pred <- predict(pcr.fit,test,ncomp=15)
pcr.test.error <- mean((data.frame(pcr.pred)-Y.test)^2)


######PLS
pls.fit <- plsr(train$Apps~.,data=train,scale=TRUE,validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred <- predict(pls.fit,test,ncomp=15)
pls.test.error <- mean((data.frame(pls.pred)-Y.test)^2)

######R-Squared test
test.avg = mean(Y.test)
lm.test.r2 = 1 - mean((Y.test - lm.predict)^2) /mean((Y.test - test.avg)^2)
ridge.test.r2 = 1 - mean((Y.test - rid_pred2)^2) /mean((Y.test - test.avg)^2)
lasso.test.r2 = 1 - mean((Y.test - lasso.pred2)^2) /mean((Y.test - test.avg)^2)
pcr.test.r2 = 1 - mean((Y.test - data.frame(pcr.pred))^2) /mean((Y.test - test.avg)^2)
pls.test.r2 = 1 - mean((Y.test - data.frame(pls.pred))^2) /mean((Y.test - test.avg)^2)
x11()
barplot(c(lm.test.r2, ridge.test.r2, lasso.test.r2, pcr.test.r2, pls.test.r2), names.arg=c("OLS", "Ridge regression", "Lasso", "PCR", "PLS"), main="R-squared")


############################################################################




