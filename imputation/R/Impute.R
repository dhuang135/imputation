# Function called Impute
# Last Updated: 3/19/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(dplyr)
library(tidyverse)

Impute<- function(data, method = "random"){
  ### Naive random imputation ###
  numCols <- dim(data)[2]
  for (i in 1:numCols){
    col <- data[,i]
    colNA <- col %>% is.na()
    col_numNA <- sum(colNA)
    if (method == "random"){
      col_imputed_values <- sample(col[!colNA] %>% unique, size=col_numNA, replace=TRUE)
    }else if(method == "mean"){
      # need to account for categorical data (fills in the most frequent factor)
      if(col %>% class() == "factor"){
        col_imputed_values <- rep((col[!colNA] %>% table)[sample(which.max(col[!colNA] %>% table), 1)] %>% names %>% as.numeric, col_numNA)
      }else if((col %>% class()) %in% c("integer", "numeric", "double")){
        col_imputed_values <- rep(mean(col[!colNA]), col_numNA)
      }
    }else if(method == "median"){
      # need to account for categorical data (fills in the most frequent factor)
      if(col %>% class() == "factor"){
        col_imputed_values <- rep((col[!colNA] %>% table)[sample(which.max(col[!colNA] %>% table), 1)] %>% names %>% as.numeric, col_numNA)
      }else if(((col %>% class()) %in% c("numeric", "integer", "double"))){
        col_imputed_values <- rep(median(col[!colNA]), col_numNA)
      }
    }else{
      stop("The method is not proper") # rephrase later
    }
    data[,i][colNA] = col_imputed_values
  }
  return(data)
}


