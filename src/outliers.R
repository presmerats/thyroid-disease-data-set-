

outliers <- function(data.mice){
  library(chemometrics)
  data2 <- data.mice[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,22,23,25)]
  data2 <- Moutlier(data2, quantile=0.975)
  return(data2)
}