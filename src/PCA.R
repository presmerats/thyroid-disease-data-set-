

# pca.func2 <- function(data.mice){
#   library(FactoMineR)
#   summary(data.mice)
#   my.pca <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23))
#   summary(data.mice$class)
#   plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))
#   
#   
# }
# 
# pca.func <- function(data.mice){
#   library(FactoMineR)
#   my.pca <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25))
#   summary(data.mice$class)
#   plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))
#   
#   
# }

pca.func2 <- function(data.mice){
  library(FactoMineR)
  names(data)
  summary(data.mice)
  my.pca <- PCA(data.mice, quali.sup = c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23))
  summary(data.mice$class)
  plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data.mice$class))
}

pca.func <- function(data,weights){
  library(FactoMineR)
  my.pca <- PCA(data, quali.sup = c(7,9),ncp=7,row.w = as.vector(weights))
  summary(data$class)
  plot(my.pca$ind$coord[,1],my.pca$ind$coord[,2], col=as.numeric(data$class))
  legend("topright", levels(data$class),text.col =as.numeric(as.factor(levels(data$class))),col =as.numeric(as.factor(levels(data$class))),pch=1 )
  
  return(my.pca)
}