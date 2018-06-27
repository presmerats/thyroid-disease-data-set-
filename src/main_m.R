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
source("PCA_rotation.R")
source("clustering.R")

data <- read.table("../dataset/thyroid0387.txt", header = FALSE, sep = ",")

# Preprocessing -------------------------------------------------
data_in <- preprocessing(data)


# MIssing values-------------------------------------------------
#data.mice <- missing.analysis(data_in)
data.mice <- missing.analysis.short(data_in)

# now binarize sex after completing missing data-----------------
data.mice <- sex.binarization(data.mice)

# errors --------------------------------------------------------
data.mice <- errors(data.mice)
data.mice2 <- errors(data.mice2)

# outliers ------------------------------------------------------
outliers.data <- outliers(data.mice)[[1]]
weights <- outliers(data.mice)[[2]]


# Target variables preparation ----------------------------------
# Transform into 7 classes, 4 classes and 2 classes problem
source("preprocessing.R")
data.7.classes <- target.extraction(data.mice,selection=7)
summary(data.7.classes$class)
data.4.classes <- target.extraction(data.mice,selection=4)
summary(data.4.classes$class)
data.2.classes <- target.extraction(data.mice,selection=2)
summary(data.2.classes$class)



# PCA -----------------------------------------------------------
source("PCA.R")
pca <- pca.func(data.mice,weights)
pca7 <- pca.func(data.7.classes, weights)
pca4 <- pca.func(data.4.classes, weights)
pca2 <- pca.func(data.2.classes, weights)

data.pca <- pca.analysis(pca4, data.mice, weights)


#  Clustering ------------------------------------------------
source("clustering.R")
clustering.analysis(pca7,data.pca,weights)


# split into training data set and test data set
set.seed(19)
N <- nrow(data.mice)
learn <- sample(1:N, round(2/3*N))
n <- as.integer(summary(data.mice[learn,]$class))


# PCA -----------------------------------------------------------
source("PCA.R")
pca <- pca.func(data.mice[learn,],weights[learn])
pca4 <- pca.func(data.4.classes[learn,], weights[learn])
data.pca <- pca.analysis(pca, data.mice, weights)
#  Clustering ------------------------------------------------
source("clustering.R")
clustering.analysis(pca4,data.pca,weights)
# PCA -----------------------------------------------------------
source("PCA.R")
pca <- pca.func(data.mice[-learn,],weights[-learn])
pca4 <- pca.func(data.4.classes[-learn,], weights[-learn])
data.pca <- pca.analysis(pca, data.mice, weights)
#  Clustering ------------------------------------------------
source("clustering.R")
clustering.analysis(pca4,data.pca,weights)




# Feature Selection ------------------------------------------
set.seed(7)
library(mlbench)
library(caret)
library(randomForest)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- randomForest(class ~ ., data=data.mice, importance=TRUE, ntree=200, proximity=FALSE,maxnodes = 10)
# estimate variable importance
importance <- varImpPlot(model)
# summarize importance
print(importance)
# plot importance
plot(importance)
# feature selection
data.select <- subset(data.mice, select = (importance[,2]>10))
colnames <- c(colnames(data.select), "class")
data.select <- cbind(data.select,data.mice$class)
colnames(data.select) <- colnames

# Removing outliers ------------------------------
data.select <-  data.select[weights == 0,]


# Classification --------------------------------------------------
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


# Removing outliers ------------------------------
data.set <-  data.mice[-which(weights == 0),]
data.7.set <- data.7.classes[-which(weights == 0),]
data.4.set <- data.4.classes[-which(weights == 0),]
data.2.set <- data.2.classes[-which(weights == 0),]

# split into training data set and test data set
set.seed(19)
N <- nrow(data.set)
learn <- sample(1:N, round(2/3*N))
n <- as.integer(summary(data.set[learn,]$class))

# CART ---------------------
library(rpart)
library(rpart.plot)

p2 = rpart(class ~ ., data=data.4.set[learn,], control=rpart.control(cp=0.001, xval=10))

# THE OBTAINED MAXIMAL TREE
p2
par(mfrow=c(1,1))
rpart.plot(p2)
# THE SEQUENCE OF TREES WITH THEIR COMPLEXITY PARAMETER AND COMPUTED ERROR IN THE TRAINING SAMPLE AND BY CROSSVALIDATION
printcp(p2)
plot(p2$cptable[,2],p2$cptable[,3],type="l",xlab="size of the tree",ylab="Relative impurity",main="R(t)")
lines(p2$cptable[,2],p2$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)
plotcp(p2)

# LETS TAKE THE COMPLEXITY PARAMETER CORRESPONDING TO THE MINIMUM CV. ERROR + 1 SD
p2$cptable = as.data.frame(p2$cptable)
ind = which.min(p2$cptable$xerror)

xerr <- p2$cptable$xerror[ind]
xstd <- p2$cptable$xstd[ind]

i = 1
while (p2$cptable$xerror[i] > xerr+xstd) i = i+1

alfa = p2$cptable$CP[i]
alfa

# AND PRUNE THE TREE ACCORDINGLY
p1 <- prune(p2,cp=alfa)
rpart.plot(p1)

# IMPORTANCE OF VARIABLES IN THE TREE DEFINITION
barplot(p1$variable.importance, main="Variables' importance", las=2)
# A NICER PLOT
library("rattle")
fancyRpartPlot(p1, caption = "Pruned Tree")
p1

asRules(p1)

# ERROR RATE IN THE LEARNING SAMPLE
pred_learn = predict(p1,data = data.4.set[learn,],type="class")
(tab_train <- table(Truth = data.4.set[learn,]$class, Pred = pred_learn))
(sum(diag(tab_train))/sum(tab_train))
1-(sum(diag(tab_train))/sum(tab_train))

# ERROR RATE IN THE TEST SAMPLE
pred_test = predict(p1, newdata = data.4.set[-learn,],type="class")
(tab_test <- table(Truth = data.4.set[-learn,]$class, Pred = pred_test))
(sum(diag(tab_test))/sum(tab_test))
1-(sum(diag(tab_test))/sum(tab_test))

# Random Forest ---------------------
library(randomForest)
set.seed(1234)
my.rf <- randomForest(class ~ ., data=data.4.set[learn,], ntree=500, proximity=FALSE)
print(my.rf)

pred_rf_learn = predict(my.rf, data = data.4.set[learn,],type="class")
(tab_rf_train <- table(Truth = data.4.set[learn,]$class, Pred = pred_rf_learn))
mean(diag(tab_rf_train)/rowSums(tab_rf_train))


# the number of normal cases are overwhelming, therefore, we need to balance the data. 
my.rf2 <- randomForest(class ~ ., data=data.4.set[learn,], ntree=500, proximity=FALSE, sampsize=c('-' =1000, hyperthyroid=155, hypothyroid=420, other=800), strata=data.4.set[learn,]$class)
print(my.rf2)

pred_rf_learn = predict(my.rf2, data = data.4.set[learn,],type="class")
(tab_rf_train <- table(Truth = data.4.set[learn,]$class, Pred = pred_rf_learn))
mean(diag(tab_rf_train)/rowSums(tab_rf_train))

# balance data
(ntrees <- round(10^seq(1,3.2,by=0.2)))
# prepare the structure to store the partial results
rf.results <- matrix (rep(0,3*length(ntrees)),nrow=length(ntrees))
colnames (rf.results) <- c("ntrees", "Recall", "OOB")
rf.results[,"ntrees"] <- ntrees
rf.results[,"Recall"] <- 0
rf.results[,"OOB"] <- 0


ii <- 1
for (nt in ntrees)
{ 
  print(nt)
  model.rf <- randomForest(class ~ ., data=data.4.set[learn,], ntree=nt, proximity=FALSE, sampsize=c('-' =1000, hyperthyroid=155, hypothyroid=420, other=800), strata=data.4.set[learn,]$class)
  rf.results[ii,"OOB"] <- model.rf$err.rate[nt,1]
  pred_rf_learn = predict(model.rf, data = data.4.set[learn,],type="class")
  tab_rf_train <- table(Truth = data.4.set[learn,]$class, Pred = pred_rf_learn)
  # get the Recall
  rf.results[ii,"Recall"] <- mean(diag(tab_rf_train)/rowSums(tab_rf_train))
  # Free memory
  gc()
  ii <- ii+1
}

rf.results
# select the number of trees
nbest=1000

# Cross-Validation
library(TunePareto)
library(randomForest)
library(e1071)

model.CV <- function (k, method) {
  CV.folds <- generateCVRuns(data.4.set[learn,]$class, ntimes=1, nfold=k, stratified=TRUE)
  train_data <- data.4.set[learn,]
  
  cv.results <- matrix (rep(0,4*k),nrow=k)
  colnames (cv.results) <- c("k","fold","TR error","VA error")

  cv.results[,"TR error"] <- 0
  cv.results[,"VA error"] <- 0
  
  for (j in 1:k) {
    print(j)
    # get VA data
    va <- unlist(CV.folds[[1]][[j]])
    if (method == "CART") {
      p2 = rpart(class ~ ., data=train_data[-va, ], control=rpart.control(cp=0.001, xval=10))
      p2$cptable = as.data.frame(p2$cptable)
      ind = which.min(p2$cptable$xerror)
      xerr <- p2$cptable$xerror[ind]
      xstd <- p2$cptable$xstd[ind]
      i = 1
      while (p2$cptable$xerror[i] > xerr+xstd) i = i+1
      alfa = p2$cptable$CP[i]
      # AND PRUNE THE TREE ACCORDINGLY
      my.model.TR <- prune(p2,cp=alfa)
    }
    else if (method == "RandomForest") {
      my.model.TR <- randomForest(class ~ ., data=train_data[-va,], ntree=1000, proximity=FALSE) 
    }
    else stop("Wrong method")
    
    # predict TR data
    if (method == "CART") {
      pred_learn = predict(my.model.TR, data = train_data[-va,],type="class")
      tab <- table(Truth = train_data[-va,]$class, Pred = pred_learn)
    } 
    else if (method == "RandomForest") {
      pred.va <- my.model.TR$predicted
      tab <- table(Truth = train_data[-va,]$class, Pred = pred.va)
    }
    
    cv.results[j,"TR error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    
    # predict VA data
    if (method == "CART") {
      pred_test = predict(my.model.TR, newdata = train_data[va,],type="class")
      tab <- table(Truth = train_data[va,]$class, Pred = pred_test)
    } 
    else if (method == "RandomForest") {
      pred.va <- predict(my.model.TR, train_data[va,], type="class")
      tab <- table(Truth = train_data[va,]$class, Pred = pred.va)
    }
    
    cv.results[j,"VA error"] <- 1-sum(tab[row(tab)==col(tab)])/sum(tab)
    cv.results[j,"fold"] <- j 
    
  }
  
  return(cv.results)
  
}

k <- 10
rf.cv <- model.CV(k, method = "RandomForest")

cart.cv <- model.CV(k, method = "CART")

ccv <- as.data.frame(cart.cv)
