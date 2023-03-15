# imputation
An Imputation Package in R. This is currently a *WORK IN PROGRESS*. As such, any help/debugging would be greatly appreciated. 

Functions:
1) `KNNimpute`: applies K-Nearest Neighbors to impute missing values. Works with both categorical and numeric data. Still refining the methodology and edge cases.
2) `RegImpute`: applied regression techniques to impute missing values. Works with both categorical and numeric data. Still refining the methodology and edge cases.

Data:
1) `missingCars`: a modified version of the `cars` dataset from the library `car`. This includes the features `speed`, `dist`, `wheels`, and `type`. Note that this dataset contains 2 factor and 2 numerical variables. 
