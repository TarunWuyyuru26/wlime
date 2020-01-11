
# CLASSIFICATION LOSSES

  # BINARY CROSS ENTROPY #######################################################
    
    binary_cross_entropy <- function(yHat, y){
      
      if(y == 1)
        return(-log(yHat))
      else
        return(-log(1 - yHat))
      
    }
    
  # CATEGORICAL CROSS ENTROPY ##################################################
    
    # categorical_cross_entropy <- function(yHat, y){
    #   return(0)
    # }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    