pca.rotation <- function(pca,data.select){
  
  print(class(pca))

  Phi <- pca$var$coord

  pc.rot <- varimax(Phi)
  Psi <- pca$ind$coord
  iden <- rownames(data.select)
  etiq <- names(data.select)

  Phi.rot <- pc.rot$loadings[1:7,]

  # Plot the rotated variables
  library(calibrate)

  Dim1 <- paste(pca$eig[1,2],"%",sep="")
  Dim1_X <- paste("Dim1", "(",round(Dim1,1),")")
  Dim2 <- paste(pca$eig[2,2],"%",sep="")
  Dim2_Y <- paste("Dim2", "(",round(Dim1,1),")")
  print(Phi.rot)
  print(pca)

  ze <- rep(0,7)
  plot(Phi.rot,xlim=c(-1,1),ylim=c(-1,1),xlab = Dim1_X, ylab = Dim2_Y)
  #plot(Phi.rot,xlim=c(-0.5,0.5),ylim=c(-0.5,0.5),xlab = Dim1_X, ylab = Dim2_Y)
  text(Phi.rot,labels=etiq[1:length(Phi.rot)], col="blue")
  arrows(ze,ze,Phi.rot[,1],Phi.rot[,2],length=0.07,col="blue")
  abline(h=0,v=0,col="gray")
  circle(1)
  
  # add an excentricity to each label
  
  # zoom in 
  ze <- rep(0,7)
  #plot(Phi.rot,xlim=c(-1,1),ylim=c(-1,1),xlab = Dim1_X, ylab = Dim2_Y)
  plot(Phi.rot,xlim=c(-0.33,0.33),ylim=c(-0.33,0.33),xlab = Dim1_X, ylab = Dim2_Y)
  text(Phi.rot,labels=etiq, col="blue")
  arrows(ze,ze,Phi.rot[,1],Phi.rot[,2],length=0.07,col="blue")
  abline(h=0,v=0,col="gray")
  circle(0.33)
  

  
  }

