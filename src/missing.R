

remove.and.impute <- function(data_in){
  
  data_without_id <- data_in
  data_in2 <- subset(data_without_id,select=c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH", "T3", "TT4", "T4U", "FTI", "referral_source", "class"))
  #(dim <- dim(data_in))
  data_wo <- data_in2
  library(mice) 
  mice <- mice(data_wo, m = 1)
  data.mice<- complete(mice)
  # write.table(as.matrix(data.mice),file="thyroid_complete.txt",sep="\t")
  #summary(data.mice)
  return(data.mice)
}

impute.all <- function(data_in){
  
  # Impute missing values
  #(dim <- dim(data_in))
  data_wo <- data_in
  library(mice) 
  mice <- mice(data_wo, m = 1)
  data.mice<- complete(mice)
  # write.table(as.matrix(data.mice),file="thyroid_complete.txt",sep="\t")
  return(data.mice)

}