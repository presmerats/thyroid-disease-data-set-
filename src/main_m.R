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
summary(data.select$class)



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
data.select <-  data.select[weigths == 0,]


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

data.mice$class <- as.factor(sapply(data.mice$class,grouping))

data.mice <- data.4.classes
summary(data.mice)

# split into training data set and test data set
set.seed(19)
N <- nrow(data.mice)
learn <- sample(1:N, round(2/3*N))
n <- as.integer(summary(data.mice[learn,]$class))

# apply Random Forest
library(randomForest)
set.seed(1234)
my.rf <- randomForest(class ~ ., data=data.mice[learn,], ntree=200, proximity=FALSE)
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

