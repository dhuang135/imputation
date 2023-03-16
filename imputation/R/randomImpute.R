# Function called randomImpute
# Last Updated: 3/16/23


# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

randomImpute<- function(data){
  ### Naive random imputation ###
  numCols <- dim(data)[2]
  for (i in 1:numCols){
    col <- data[,i]
    colNA <- col %>% is.na()
    col_numNA <- sum(colNA)
    col_imputed_values <- sample(col[!colNA] %>% unique, size=col_numNA, replace=TRUE)
    data[,i][colNA] = col_imputed_values
  }
}
