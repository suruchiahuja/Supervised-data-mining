library(ggplot2)
library(MASS)
data(Boston)

#pairwise scatterplots
pairs(Boston[ , ])

#predictors associated with crime rate per capita
cor(crim , zn)#negative value
cor(crim , indus)#0.406
cor(crim , chas)#negative value
cor(crim , nox)#0.420
cor(crim , rm)#negative value
cor(crim , age)#negative value
cor(crim , dis)#negative value
cor(crim , rad)#o.625
cor(crim , tax)#0.582
cor(crim , ptratio)#0.289
cor(crim , black)#negative value
cor(crim , lstat)#0.4556
cor(crim , medv)#negative value


#number of suburbs having high crime rates

attach(Boston)
suburbs <- Boston[ , c(1,10,11)]
View(Boston)
 x <- (suburbs$Boston >8 & suburbs$tax >600 & suburbs$ptrartio >20)
 x <- as.numeric(x)
 summary(x)

crims <- sort(crim, decreasing = TRUE)
taxs <- sort(tax, decreasing = TRUE)
ptratios <- sort(ptratio, decreasing = TRUE)
sorted <- cbind(crims,taxs,ptratios)
View(sorted)
list <- sorted[1:76, ]
View(list)
hist(list)

range(crim)

range(tax)

range(ptratios)


#Number of suburbs that average more than seven per dwelling


dwellers <- Boston[,6]
more7 <- dwellers>7
summary(more7)
   
more8 <- rme >8
summary(more8)

 