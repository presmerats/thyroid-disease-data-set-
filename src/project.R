if(!require(rstudioapi)) {
  install.packages("rstudioapi")
  require(rstudioapi)
}
setwd(dirname(getActiveDocumentContext()$path))

data <- read.table("../dataset/thyroid0387.txt", header = FALSE, sep = ",")


data_without_id <- data
# Remove the id 
data_without_id[, 30] <- (substr(data_without_id[, 30], 1, 1))
data_without_id[, 30] <- as.factor(data_without_id[, 30])
# Rename the columns
colnames(data_without_id) <- c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", 
                               "TSH_measured", "TSH", "T3 measured", "T3", "TT4_measured", "TT4", "T4U_measured", "T4U", 
                               "FTI_measured", "FTI", "TBG_measured", "TBG", "referral_source", "class")


# mean of the ind. that are healthy
min(data_in$TBG)
data_in$TBG[ data_in$TBG == "?" ] <- NA
min(data_in$TBG)
summary(data_in)


# strange values not appearing in TBG ------------------------------------------




# MIssing values------------------------------------------------------------------------------------------


data.numeric <- function(data,varname){
  #colnames(data)
  varcolum = data[,which(colnames(data) == varname)]
  varcolum[varcolum == '?'] <- NA
  data[,which(colnames(data) == varname)] <- as.numeric(as.character(varcolum))
  data[,which(colnames(data) == varname)] <- as.numeric( data[,which(colnames(data) == varname)])
  return(data)
}


# option 1) removing TBG-----------

summary(data_without_id)

data_in <- subset(data_without_id,select=c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", 
                                           "TSH", "T3", "TT4", "T4U", "FTI", "referral_source", "class"))

summary(data_in)

# replace ? with NA
data_in$sex[ data_in$sex == "?" ] <- NA
# drop unused levels
data_in$sex <- droplevels(data_in$sex)

data_in <- data.numeric(data_in,"TSH")
data_in <- data.numeric(data_in,"T3")
data_in <- data.numeric(data_in,"TT4")
data_in <- data.numeric(data_in,"T4U")
data_in <- data.numeric(data_in,"FTI")

# 
# data_in3 <- data_in
# data_in3$TSH[ data_in$TSH == "?" ] <- NA
# data_in3$T3[ data_in$T3 == "?" ] <- NA
# data_in3$TT4[ data_in$TT4 == "?" ] <- NA
# data_in3$T4U[ data_in$T4U == "?" ] <- NA 
# data_in3$FTI[ data_in$FTI == "?" ] <- NA
# data_in3$TBG[ data_in$TBG == "?" ] <- NA
# 
# # Transform some viraibles from categorical to continuous
# data_in3$TSH <- as.numeric(data_in$TSH)
# data_in3$T3 <- as.numeric(data_in$T3)
# data_in3$TT4 <- as.numeric(data_in$TT4)
# data_in3$T4U <- as.numeric(data_in$T4U)
# data_in3$FTI <- as.numeric(data_in$FTI)




summary(data_in)
# Impute missing values
(dim <- dim(data_in))
data_wo <- data_in
library(mice) 
mice <- mice(data_wo, m = 1)
data.mice<- complete(mice)
# write.table(as.matrix(data.mice),file="thyroid_complete.txt",sep="\t")
summary(data.mice)

# PCA
library(FactoMineR)

my.pca <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23))

summary(data.mice$class)
plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))

# option 2) doing MICE-------

data_in <- subset(data_without_id,select=c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", 
                                           "TSH", "T3", "TT4", "T4U", "FTI", "referral_source","TBG_measured", "TBG", "class"))


summary(data_in)

# replace ? with NA
data_in$sex[ data_in$sex == "?" ] <- NA
# drop unused levels
data_in$sex <- droplevels(data_in$sex)

data_in <- data.numeric(data_in,"TSH")
data_in <- data.numeric(data_in,"T3")
data_in <- data.numeric(data_in,"TT4")
data_in <- data.numeric(data_in,"T4U")
data_in <- data.numeric(data_in,"FTI")
data_in <- data.numeric(data_in,"TBG")
# 
# data_in3 <- data_in
# data_in3$TSH[ data_in$TSH == "?" ] <- NA
# data_in3$T3[ data_in$T3 == "?" ] <- NA
# data_in3$TT4[ data_in$TT4 == "?" ] <- NA
# data_in3$T4U[ data_in$T4U == "?" ] <- NA 
# data_in3$FTI[ data_in$FTI == "?" ] <- NA
# data_in3$TBG[ data_in$TBG == "?" ] <- NA
# 
# # Transform some viraibles from categorical to continuous
# data_in3$TSH <- as.numeric(data_in$TSH)
# data_in3$T3 <- as.numeric(data_in$T3)
# data_in3$TT4 <- as.numeric(data_in$TT4)
# data_in3$T4U <- as.numeric(data_in$T4U)
# data_in3$FTI <- as.numeric(data_in$FTI)




summary(data_in)
# Impute missing values
(dim <- dim(data_in))
data_wo <- data_in
library(mice) 
mice <- mice(data_wo, m = 1)
data.mice<- complete(mice)
# write.table(as.matrix(data.mice),file="thyroid_complete.txt",sep="\t")
summary(data.mice)

# PCA
library(FactoMineR)

my.pca <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25))

summary(data.mice$class)
plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))





# MCA
my.mca<- MCA(data.mice,quanti.sup=c(1,17,18,19,20,21),level.ventil = 0,ncp=dim[2])
plot(mca.car$eig$eigenvalue,type="l",ylab = "Percentage of eigenvalues")

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
