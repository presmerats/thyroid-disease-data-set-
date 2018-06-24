

mca.func <- function(data.mice){
  my.mca<- MCA(data.mice,quanti.sup=c(1,17,18,19,20,21),level.ventil = 0,ncp=dim[2])
  plot(mca.car$eig$eigenvalue,type="l",ylab = "Percentage of eigenvalues")
  
}