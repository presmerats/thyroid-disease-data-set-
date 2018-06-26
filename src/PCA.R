

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
  par(mfrow=c(1,1))
  my.pca <- PCA(data, quali.sup = c(22,24),ncp=7,row.w = as.vector(weights))
  summary(data$class)
  Dim1 <- my.pca$ind$coord[,1]
  Dim2 <- my.pca$ind$coord[,2]
  plot(Dim1,Dim2, col=as.numeric(data$class), main="Individuals factor map (PCA)")
  legend("topright", levels(data$class),text.col =as.numeric(as.factor(levels(data$class))),col =as.numeric(as.factor(levels(data$class))),pch=1 )
  

  
  return(my.pca)
}

contributions <- function(my.pca){
  
  # contribution of each var to the pc's
  # -> standardized so all have variance 1, all have the same contribution to the total inertia
  
  # show the contributions for the first and second dim
  my.pca$var$contrib[,c(1,2)]
  
}

pca.analysis <- function(pca, data.mice, weights){
  
  # significant dimensions---------------------
  pca$eig[,3]
  # -> 90% inertia 17 princicpal components
  # plot, elbow rule
  plot(pca$eig[,1],type="l",
       xlab="Eigenvectors", ylab="Eigenvalues")
  
  
  # Var contribution-------------------------
  # to see the contribution of each var to the first 2 pc
  contribs <- contributions(pca)
  contribs <- data.frame(contribs)
  # contribs sorted by first Dim
  contribs[order(-contribs$Dim.1),]
  # contribs sorted by second Dim
  contribs[order(-contribs$Dim.2),]
  
  # Var representation------------------------
  # best represented variables in the first factorial plane
  n = nrow(pca$var$cos2)
  # worst_var = sort(rowSums(pca$var$cos2[,1:2], na.rm = FALSE, dims = 1))[1]
  # worst_var
  best_vars = sort(rowSums(pca$var$cos2[,1:2], na.rm = FALSE, dims = 1))
  best_vars_dim1 <- pca$var$cos2[order(-pca$var$cos2[,1]),1:2]
  best_vars_dim1
  best_vars_dim2 <- pca$var$cos2[order(-pca$var$cos2[,2]),1:2]
  best_vars_dim2
  
  # target modalities represatives ----------------------
  # well represented modalities of the target in first fact. plane
  pca$quali.sup$v.test
  # we remove the modalities of the referral
  non.interesting <- c(1,2,3,4,5,6)
  pca$quali.sup$v.test[-non.interesting,1:2]
  
  n = nrow(pca$quali.sup$v.test[-non.interesting,1:2])
  sorted_modalities = sort(rowSums(pca$quali.sup$v.test[-non.interesting,1:2], na.rm = FALSE, dims = 1))
  best_sorted_modalities = sort(rowSums(pca$quali.sup$v.test[-non.interesting,1:2], na.rm = FALSE, dims = 1))
  best_sorted_modalities
  
  data.pca <- pca$ind$coord
  
  source("PCA_rotation.R")
  par(mfrow=c(1,1))
  pca.rt <- pca.rotation(pca, data.mice)
  varimax(pca$var$cor[,1:7])$loadings
  
  
  return(data.pca)
}