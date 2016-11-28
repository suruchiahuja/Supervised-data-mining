library(ElemStatLearn)
ls("package:ElemStatLearn")

#converting matrix test set into data frame test set
zip.test <- as.data.frame(zip.test)
#converting matrix train set into data frame train set
zip.train <- as.data.frame(zip.train)

#k=2,3 attaching to test set
zip.test.23 <- zip.test[zip.test$V1 == 2 | zip.test$V1 == 3, ]
#k=2,3 attaching to train set
zip.train.23 <- zip.train[zip.train$V1 == 2 | zip.train$V1 == 3, ]


#knn

require(class)
k<- c(1,3,5,7,15)
error <- rep(NA, length(k))
for (i in 1:length(k)){
  KNN <- knn(zip.train.23[, -1], zip.test.23[, -1], zip.train.23$V1, k[i])
  y_true = as.numeric(zip.test.23[,1])-1
  y_test = as.numeric(KNN)
  error[i] = (1/length(y_test))*sum(abs(y_test-y_true))
          
  
}



#linear regression

model <- lm(V1 ~ ., data=zip.train.23)
coef(model)
y.true <- zip.test.23$V1
zip.test.23$v1 <- NULL
#x <- rep(x = 1, length = nrow(zip.test.23))
ynew <- data.frame()
ynew <- cbind(1, zip.test.23)
L.error <-mean(ynew != length(zip.train.23)+1)


#compile result in a table

results <- matrix(c(L.error, error), ncol=1)
colnames(results) <- c("Error Rate")
rownames(results) <- c("Linear Regression", paste("K-nn with k=", k))
results
plot(c(1, 15), c(0, 1.1 * max(error)), type = "n", main = "Comparing Classifiers", 
     ylab = "Error Rate", xlab = "k")
abline(h = L.error, col = 2, lty = 3)
points(k, error, col = 4)
lines(k, error, col = 4, lty = 2)

