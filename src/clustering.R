clustering.analysis <- function(pca,data.mice, weights, nsd=7){
  
  # 4.Perform a hierarchical clustering with the significant factors, decide the number of
  # final classes to obtain and perform a consolidation operation of the clustering. 
  Psi <- pca$ind$coord[,c(1:nsd)]
  d <- dist(Psi, method = "euclidean")
  hc <- hclust(d, method = "ward.D2")
  
  # determining the greatest height jump and number of clusters
  height.diff <- c(hc$height,0) - c(0,hc$height)
  before.maxi <- which.max(height.diff[c(1:(length(height.diff)-2))]) -1
  nc = length(hc$height) - before.maxi + 1
  height.line <- (hc$height[before.maxi + 1] + hc$height[before.maxi ])/2
  
  plot(hc)
  abline(a=height.line,b=0,col="red",)
  barplot(hc$height[(length(hc$height) - 100):length(hc$height)], xlab = "Height histogram")
  abline(a=height.line,b=0,col="red",)
  
  # visualizing clusters
  nc = 10
  c3 <- cutree(hc,nc)
  
  
  iden <- rownames(Psi)
  plot(Psi[,1],Psi[,2],type="n",main="Clustering in 10 classes")
  text(Psi[,1],Psi[,2],col=c3,labels=iden,cex = 0.6)
  abline(h=0,v=0,col="gray")
  legend("topright",as.character(c(1:10)),pch=20,col=c(1:10))
  
  
  
  # computation of the centroids of clusters
  cdg <- aggregate(Psi,list(c3),mean)[,2:(nsd+1)]
  
  
  
  # compute the quality of the hierarchical clustering
  n <- length(Psi[1])
  Bss <- sum(rowSums(cdg^2)*as.numeric(table(c3)))
  Tss <- sum(rowSums(Psi^2))
  optimization.criterion.before <- 100*Bss/Tss
  
  
  # K-means computation
  
  k_def <- kmeans(Psi,centers=cdg)
  Bss <- sum(rowSums(k_def$centers^2))
  Wss <- sum(k_def$withinss)
  100*Bss/(Bss+Wss)
  # printing final clusters
  plot(Psi,type="none",main="Kmeans Clustering")
  text(Psi,labels=iden,col=k_def$cluster)
  abline(h=0,v=0,col="gray")
  legend("bottomright",as.character(c(1:10)),pch=20,col=c(1:10))
  points(k_def$centers, pc=21, col = "yellow", bg="red", cex = 1.5)
  text(k_def$centers - rep(0.25,length(k_def$centers)),labels=as.character(c(1:10)),col="yellow",cex=1.6,pch=10)
  
  # Quality of the Kmeans clustering
  Bss = k_def$betweenss
  Wss = k_def$tot.withinss
  optimization.criterion.after <- 100*Bss/(Bss+Wss)
  
  
  # 5. Using the function catdes, interpret the obtained clusters and represent them in the first factorial display.
  
  # remove outliers? not needed here
  
  # make class be considered categorical and not continuous 
  
  
  # finally we call the catdes to compute the v-tests for the different variables
  cd <- catdes(cbind(as.factor(k_def$cluster),data.mice), num.var=1, proba=0.05, row.w=NULL)
  
  # we now proceed to represent the clusters in the first factorial plane
  plot(Psi,type="none",main="Clustering in 10 classes and initial centroids")
  text(Psi,labels=iden,col=c3)
  abline(h=0,v=0,col="gray")
  legend("topright",as.character(c(1:10)),pch=20,col=c(1:10))
  points( cdg, pc=21, col = "yellow", bg="black", cex = 1.5)
  # displace the cetroid label
  cdg2 <- cdg
  cdg2 <- cdg2 - rep(1.2,length(cdg2[,2]))
  text(cdg2 ,labels=as.character(c(1:10)),col="yellow",cex=1.1, pch=4)
  
  
  library(cluster)
  clusplot(data.mice, k_def$cluster, main='2D representation of the Cluster solution',
           color=TRUE, shade=TRUE,
           labels=10, lines=0, 
           col.p = data.mice$class,
           col.clus =k_def$cluster )
  legend("bottomleft",as.character(c(1:10)),pch=20,col=c(1:length(k_def$centers)))
  
  
  
  
}