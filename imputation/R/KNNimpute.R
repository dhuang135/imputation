# Function called KNNimpute
#

# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(tidyverse)


#Test data: Use only while writing package
xvalues <- sample(1:50, 10)
yvalues <- sample(1:2, 10, replace = TRUE)
n <- 5 #need to set n to a variable!

#replaces values to NA in cars dataset by row index, col index
for(i in 1:n){
  cars[xvalues[i], yvalues[i]] = NA
}


type <- sample(1:3, 50, replace = TRUE)

cars <- cars %>% cbind(type)
cars <- cars %>% mutate(type = as.factor(type))


N <- (cars_clean %>% dim)[1] #cars_clean not initialized

data = cars
data_clean = cars %>% drop_na()

N <- (data_clean %>% dim)[1] #to get row number of non-NA entries

KNNimputation <- function(data, k = 10, method = "mean", split_categorical = FALSE){
  # This is a KNN imputation function


  ### Distance Metrics ###
  
  #NORM FUNCTION DOES NOT WORK WITH FACTORS 
  norm <- function(x){
    return(sqrt(sum(x^2)))
  }
  # Consider other norms

  ### Test Cases ###
  ############### Initialized and later delete #################
  #k = 5
  #data <- data_clean[1:3][1,] for testing purposes only
  
  if(sum(complete.cases(data)) <= k){
    stop("Not enough complete cases (cases without NAs).")
  }

  if(!all(sapply(data, class) %in% c("factor","numeric"))){
    stop("Not all columns are of form factor or numeric!")
  }

  if(dim(data %>% is.na) %>% is.null){ # our data is one-dimensional: handle this case later
    # determine if the single vector is categorical/numeric
    # then appropriate calculation
    return()
  }

  # Test: drops full NA rows
  test = (dim(data)[2] == rowSums(data %>% is.na)) == FALSE #number of columns equal to sum of each row with 1 being NA value
  if(!all(test)){
    n = sum(!all(test))
    warning("Data frame contained rows with full NA's. " , as.character(n), " rows dropped.")
  }



  ### Method of Aggregation (for numeric variables) ###

  ############# Initialized and later delete ##############
  #method = "mean"

  if(method == "mean"){
    agg = mean
  }else if(method == "median"){
    agg = median
  }else{
    # weighted average?
  }

  ### Imputation ###
  # Also consider other ways to impute data while combining factors and numeric columns.
  # 1) Quantitative Split
  data_numeric <- data[,sapply(data, class) == "numeric"]
  data_numeric_clean <- data_numeric %>% drop_na()
  
  #when is the dim(data_numeric %>% is.na) returning null?
  if (dim(data_numeric %>% is.na) %>% is.null){
    row_has_NA <- which(data_numeric %>% is.na)
  }else{
    row_has_NA <- which((rowSums(data_numeric %>% is.na) %>% as.logical)) # we have a vector of indexes that contain an NA value
  }
  
  
  #row_has_NA vector creation is conditional, will run an error when the above IF statement is false due to no initialization
  numNA <- length(row_has_NA)
  imputed_values = rep(9999, numNA)
  screen <- (data_numeric %>% is.na) == FALSE
  for(i in 1:numNA){
    x <- (data_numeric[row_has_NA[i], ])[screen[row_has_NA[i], ]] # set we are trying to minimize distance over
    X <- data_numeric_clean[screen[row_has_NA[i], ]] # actual thing we are comparing
    N <- dim(X)[1]
    differenced_data <- X - x[rep(1, N),]
    distance_metric <- apply(differenced_data, 1, norm) # later on make is such that our function changes based off of command given
    minKNorm <- sort(distance_metric)[1:k] # gets k values with the smallest "distance"
    Kcluster_index <- which(distance_metric %in% minKNorm) # gets indeces of smallest distances
    # How to address ties
    # If the largest distance is tied (Better way to phrase this)
    if(length(Kcluster) > k){
      # the biggest value is tied
    }

    # Weighted Average Method
    
    if(method == "weightAvg"){
      imputed_values[i] = agg(exp(-distance_metric[Kcluster_index]) * X[Kcluster_index, ])
    }else{
      imputed_values[i] = agg(X[Kcluster_index, ]) # the KNN imputed value
    }
  }
  data_numeric[data_numeric %>% is.na()] = imputed_values

  # 2) Categorical Split
  # could also implement Jaccard index
  data_categorical <- data[,sapply(data, class) == "factor"]
  data_categorical_clean <- data[,sapply(data, class) == "factor"]
  if (dim(data_categorical %>% is.na) %>% is.null){
    row_has_NA <- which(data_categorical %>% is.na)
  }else{
    row_has_NA <- which((rowSums(data_categorical %>% is.na) %>% as.logical)) # we have a vector of indeces that contain an NA value
  }
  numNA <- length(row_has_NA)
  imputed_values = rep(9999, numNA)
  screen <- (data %>% is.na) == FALSE
  for(i in 1:numNA){

    x <- (data_numeric[row_has_NA[i], ])[screen[row_has_NA[i], ]] # set we are trying to minimize distance over
    X <- data_numeric_clean[screen[row_has_NA[i], ]] # actual thing we are comparing
    N <- dim(X)[1]
    differenced_data <- X - x[rep(1, N),]
    distance_metric <- apply(differenced_data, 1, norm) # later on make is such that our function changes based off of command given
    minKNorm <- sort(distance_metric)[1:k] # gets k values with the smallest "distance"
    Kcluster_index <- which(distance_metric %in% minKNorm) # gets indeces of smallest distances

    # How to address ties???
    if(length(Kcluster) > k){
      # When the biggest value is tied, need to figure out solution
    }
    # Aggregation function
    imputed_values[i] = which.max(table(data_clean[Kcluster_index, screen[row_has_NA[i], ] == FALSE] )) # the KNN imputed value
  }
  data_categorical[data_categorical %>% is.na()] = imputed_values

  imputedData <- data_numeric %>% cbind(data_categorical)
  return(imputedData)
}

