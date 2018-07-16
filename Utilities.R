# ===============================================================================================================
# getFormula  - will retrieve the formula based on a data set
# 
# Parameter:
#   data  - dataset
#   label - label 
# ===============================================================================================================

getFormula <- function(data = data, label = label) {
  
  # label Variable
  labelVar <- label
  
  # separate measure Variable y column from rest 
  groupVariables <- setdiff(colnames(data), list(label))
  
  # create formula for model 
  formula <- as.formula(paste(labelVar, paste(groupVariables, collapse = ' + '), sep = ' ~ '))
  
}

# ===============================================================================================================
# dmode  - will retrieve the mode of a given desity function (data) 
# 
# Parameter:
#   vector - label 
# ===============================================================================================================

dmode <- function(x) {
  den <- density(x, kernel = c("gaussian"))
  ( den$x[den$y == max(den$y)] )   
}  


# ===============================================================================================================
# Computing splitTrainTestSet 
# 
# Split the data set into a training and test set based on the given split ratio
# 
# Parameter:
#   data  - data set
#   split - split ratio
# ===============================================================================================================

splitTrainTestSet <- function(data = data, split = 0.5) {
  
  sub <- sample(nrow(data), floor(nrow(data) * split))
  train <- data[sub,]
  test <- data[ - sub,]
  
  split.set <- list(train = train, test = test)
  
  return(split.set)
}

