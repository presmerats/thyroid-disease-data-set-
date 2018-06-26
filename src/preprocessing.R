
data.numeric <- function(data,varname){
  #colnames(data)
  varcolum = data[,which(colnames(data) == varname)]
  varcolum[varcolum == '?'] <- NA
  data[,which(colnames(data) == varname)] <- as.numeric(as.character(varcolum))
  data[,which(colnames(data) == varname)] <- as.numeric( data[,which(colnames(data) == varname)])
  return(data)
}


preprocessing <- function(data){
  
  data_without_id <- data
  # Remove the id 
  data_without_id[, 30] <- (substr(data_without_id[, 30], 1, 1))
  data_without_id[, 30] <- as.factor(data_without_id[, 30])
  # Rename the columns
  colnames(data_without_id) <- c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH_measured", "TSH", "T3 measured", "T3", "TT4_measured", "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured", "TBG", "referral_source", "class")
  
  data_in <- subset(data_without_id,select=c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", 
                                             "TSH", "T3", "TT4", "T4U", "FTI", "referral_source","TBG", "class"))
  
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
  
  # binary data transformation
  data_in$on_thyroxine <- as.numeric(data_in$on_thyroxine)-1
  data_in$query_on_thyroxine <- as.numeric(data_in$query_on_thyroxine)-1
  data_in$on_antithyroid_medication <- as.numeric(data_in$on_antithyroid_medication)-1
  data_in$sick <- as.numeric(data_in$sick)-1
  data_in$pregnant <- as.numeric(data_in$pregnant)-1
  data_in$thyroid_surgery <- as.numeric(data_in$thyroid_surgery)-1
  data_in$I131_treatment <- as.numeric(data_in$I131_treatment)-1
  data_in$query_hypothyroid <- as.numeric(data_in$query_hypothyroid)-1
  data_in$query_hyperthyroid <- as.numeric(data_in$query_hyperthyroid)-1
  data_in$lithium <- as.numeric(data_in$lithium)-1
  data_in$goitre <- as.numeric(data_in$goitre)-1
  data_in$tumor <- as.numeric(data_in$tumor)-1
  data_in$hypopituitary <- as.numeric(data_in$hypopituitary)-1
  data_in$psych <- as.numeric(data_in$psych)-1
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
  
  #summary(data_in)
  
  return(data_in)  
}


sex.binarization <- function(data_in){
  data_in$sex <- as.numeric(data_in$sex)-1
  return(data_in)
}

preprocessing_old <- function(data){
  
  data_without_id <- data
  # Remove the id 
  data_without_id[, 30] <- (substr(data_without_id[, 30], 1, 1))
  data_without_id[, 30] <- as.factor(data_without_id[, 30])
  # Rename the columns
  colnames(data_without_id) <- c("age", "sex", "on_thyroxine", "query_on_thyroxine", "on_antithyroid_medication", "sick", "pregnant", "thyroid_surgery", "I131_treatment", "query_hypothyroid", "query_hyperthyroid", "lithium", "goitre", "tumor", "hypopituitary", "psych", "TSH_measured", "TSH", "T3 measured", "T3", "TT4_measured", "TT4", "T4U_measured", "T4U", "FTI_measured", "FTI", "TBG_measured", "TBG", "referral_source", "class")
  
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

  #summary(data_in)

  return(data_in)  
}


target.extraction <- function(data.select, selection){
  
  levels.original <- levels(data.select$class)
  levels.add <- c(levels.original, 
                  "hyperthyroid","hypothyroid","binding",
                  "concurrent_i","replacement_t","antithyroid_t",
                  "discordant","other","positive")
  levels(data.select) <- levels.add
  
  summary(data.select$class)
  
  # 7-class target
  class7 <- data.select$class
  levels(class7) <- levels.add
  # 4-class target
  class4 <- data.select$class
  levels(class4) <- levels.add
  # 2-class target
  class2 <- data.select$class
  levels(class2) <- levels.add
  
  for (i in 1:length(class7)){
    if (class7[i] %in% c("A","B","C","D")){
      class7[i] <- "hyperthyroid"  
    } else if (class7[i] %in% c("E","F","G","H")){
      class7[i] <- "hypothyroid"  
    }  else if (class7[i] %in% c("I","J")){
      class7[i] <- "binding"  
    } else if (class7[i] %in% c("K")){
      class7[i] <- "concurrent_i"
    } else if (class7[i] %in% c("L","M","N")){
      class7[i] <- "replacement_t"
    } else if (class7[i] %in% c("O","P","Q")){
      class7[i] <- "antithyroid_t"
    } else if (class7[i] %in% c("R","S","T")){
      class7[i] <- "discordant"
    } else if (!(class7[i] %in%  c("-")) ) {
      class7[i] <- "other"  
    }
    
    if (class4[i] %in% c("A","B","C","D")){
      class4[i] <- "hyperthyroid"  
    } else if (class4[i] %in% c("E","F","G","H")){
      class4[i] <- "hypothyroid"  
    } else if (!(class4[i] %in%  c("-")) ){
      class4[i] <- "other"  
    }
    
    if (!(class2[i] %in%  c("-"))){
      class2[i] <- "positive"
    }
  }
  
  if (selection == 7){
    data.select$class <- class7
    levels2 <- levels(class7)
    levels(data.select$class) <- levels2
  } else if (selection == 4){
    data.select$class <- class4
    levels2 <- levels(class4)
    levels(data.select$class) <- levels2
  } else if (selection == 2){
    data.select$class <- class2
    levels2 <- levels(class2)
    levels(data.select$class) <- levels2
  } else {
    # by default 4 classes
    data.select$class <- class4
    levels2 <- levels(class4)
    levels(data.select$class) <- levels2
  }
  
  data.select$class <-  droplevels(data.select$class)
  return(data.select)
  
}