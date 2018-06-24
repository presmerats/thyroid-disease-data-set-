if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  require(rstudioapi)
}
setwd(dirname(getActiveDocumentContext()$path))


# source scripts

source("preprocessing.R")
source("missing.R")
source("errors.R")
source("outliers.R")
source("PCA.R")
source("MCA.R")

data <- read.table("../dataset/thyroid0387.txt", header = FALSE, sep = ",")

# Preprocessing ---------------------------------------------
data_in <- preprocessing(data)


# MIssing values------------------------------------------------------------------------------------------

# option 1) removing TBG-----------
data.mice <- remove.and.impute(data_in)
# PCA for comparison
library(FactoMineR)
summary(data.mice)
my.pca1 <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23))
summary(data.mice$class)
plot(my.pca1$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))

# option 2) doing MICE-------
data.mice2 <- impute.all(data_in)
# PCA for comparison
library(FactoMineR)
my.pca2 <- PCA(data.mice2, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25))
summary(data.mice2$class)
plot(my.pca2$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))

# option 3) doin MICE for conditions, 24 for healty-----
data.mice3 <- impute.condition(data_in)
# PCA for comparison
library(FactoMineR)
my.pca3 <- PCA(data.mice3, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25))
summary(data.mice3$class)
plot(my.pca3$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))

# option 4) doin MICE for conditions, 24 for healty-----
data.mice4 <- impute.condition2(data_in)
# PCA for comparison
library(FactoMineR)
my.pca4 <- PCA(data.mice4, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25))
summary(data.mice4$class)
plot(my.pca4$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))


# errors -------------------------------------------------
data.mice <- errors(data.mice2)

# outliers ----------------------------------------------------------------
outliers.data <- outliers(data.mice)




# PCA
pca.func(data.mice)

# MCA
mca.func(data.mice)

# clustering 

# interpretation

# Classification
# firt grouping 
grouping <- function(doc) {
  doc <- gsub("A","hyperthyroid", doc)
  doc <- gsub("B","hyperthyroid", doc)
  doc <- gsub("C","hyperthyroid", doc)
  doc <- gsub("D","hyperthyroid", doc)
  doc <- gsub("E","hypothyroid", doc)
  doc <- gsub("F","hypothyroid", doc)
  doc <- gsub("G","hypothyroid", doc)
  doc <- gsub("H","hypothyroid", doc)
  doc <- gsub("I","protein", doc)
  doc <- gsub("J","protein", doc)
  doc <- gsub("K","general", doc)
  doc <- gsub("L","replacement", doc)
  doc <- gsub("M","replacement", doc)
  doc <- gsub("N","replacement", doc)
  doc <- gsub("O","antithyroid", doc)
  doc <- gsub("P","antithyroid", doc)
  doc <- gsub("Q","antithyroid", doc)
  doc <- gsub("R","miscellaneous", doc)
  doc <- gsub("S","miscellaneous", doc)
  doc <- gsub("T","miscellaneous", doc)
  return(doc)
}

data.mice$class <- as.factor(sapply(data.mice$class,grouping))

# split into training data set and test data set
N <- nrow(data.mice)
learn <- sample(1:N, round(2/3*N))
n <- as.integer(summary(data.mice[learn,]$class))

# apply Random Forest
library(randomForest)
set.seed(1234)
my.rf <- randomForest(class ~ ., data=data.mice[learn,], sampsize=c(660,n[2:8]), importance=TRUE, xtest=data.mice[-learn,-23], ytest=data.mice[-learn,23],ntree=200, proximity=FALSE,maxnodes = 8)
print(my.rf)
# the number of normal cases are overwhelming, therefore, we need to balance the data. 

# balance data
num <- seq(10,n[1],50)
rf.results=matrix(rep(0,2*length(num)), nrow=length(num))
colnames (rf.results) <- c("normal count","test accuracy")
rf.results[,"normal count"] <- num
rf.results[,"test accuracy"] <- 0

i <- 1
for (nn in num) 
{
  model.rf <- randomForest(class ~ ., data=data.mice[learn,], sampsize=c(nn,n[2:8]), importance=TRUE, xtest=data.mice[-learn,-23], ytest=data.mice[-learn,23],ntree=200, proximity=FALSE,maxnodes = 8)
  rf.results[i,"test accuracy"] <- (sum(diag(model.rf$test$confusion)))/(sum(model.rf$test$confusion))
  
  i <- i+1
}

rf.results

# select the number of trees


##########################################################################################################################
library(e1071)
set.seed(1234)
my.ksvm <- svm(data.mice[learn,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23)],data.mice[learn,23], probability=TRUE,type="C-classification", cost=1, kernel="radial", scale = TRUE)

# a) training error
pred.ksvm.train1 <- predict(my.ksvm, data.mice[learn,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23)], decision.values = TRUE, probability = TRUE)
(ct <- table(Truth=data.mice[learn,23], Pred=pred.ksvm.train1))
round(100*(1-sum(diag(ct))/sum(ct)),2)
# train error is 16.57%
# b) test error 
pred.ksvm.test1 <- predict(my.ksvm, data.mice[-learn,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23)], decision.values = TRUE, probability = TRUE)
(ct <- table(Truth=data.mice[-learn,23], Pred=pred.ksvm.test1))
round(100*(1-sum(diag(ct))/sum(ct)),2)
# test error is 17.8%
