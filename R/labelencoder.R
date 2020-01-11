
# Instant Label Encoding
labelEncoder_instante <- function(column_vec){
  
  column_vec = as.factor(column_vec)
  
  levels(column_vec) = seq(1:length(levels(column_vec)))
  
  return(as.integer(column_vec))
}

# Label Encoding which can be utilized for the predicting test data as well
labelEncoder <- function(column_vec){
  
  before_transform_levels = levels(column_vec)
  
  levels(column_vec) = seq(1:length(unique(column_vec)))
  
  after_transform_levels = levels(column_vec)
  
  final_object = list("output" = as.integer(column_vec),
                      "levels_before_transformation" = before_transform_levels,
                      "levels_after_transformation" = after_transform_levels)
  
  class(final_object) = 'labelEncoderMethod'
  
  return(final_object)
}

predict.labelEncoderMethod <- function(new_vec, object, handleNewLevels = TRUE, verbose = TRUE){
  
  new_levels = levels(new_vec)
  trained_levels = object$levels_before_transformation
  
  if((all(new_levels %in% trained_levels)) == FALSE){
    if(handleNewLevels == FALSE){
      stop(paste("New Data has new levels which are not trained, Enabling ignoreNewLevels to TRUE will make the unseen levels to 0"))
    }
  }
  
  unseen_levels = setdiff(new_levels, trained_levels)
  
  if(length(unseen_levels) == 0){
    
    if(verbose == TRUE){
      print("No new levels !!")
    }
    
    levels(new_vec) = object$levels_after_transformation
    
    return(new_vec)
    
  }else if(length(unseen_levels) != 0){
    
    unseen_levels_idxs = which(x = levels(new_vec) %in% unseen_levels,arr.ind = TRUE)
    
    levels(new_vec)[unseen_levels_idxs] = 0
    
    trained_levels_idxs = which(x = levels(new_vec) %in% trained_levels,arr.ind = TRUE)
    
    levels(new_vec)[trained_levels_idxs] = object$levels_after_transformation
    
    return(new_vec)
    
  }
}


# q = labelEncoder(column_vec = iris$Species)
# q$output 
# q
# 
# new_vec = as.factor(c("setosa","versicolor","virginica","setosa","versicolor","virginica",
#                       "uodudu","setosa","versicolor","virginica","sappdu","pichakaya"))
# 
# new_vec = as.factor(c("setosa","versicolor","virginica","setosa","versicolor","virginica",
#                       "setosa","versicolor","virginica"))
# 
# new_vec = as.factor(c("setosa","versicolor","versicolor","setosa","versicolor","versicolor",
#                       "setosa","versicolor","versicolor"))
# 
# 
# w = predict(object = q,new_vec = new_vec,handleNewLevels = TRUE,verbose = TRUE)
# w

