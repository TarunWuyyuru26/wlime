
# SIGMOID FUNCTION ###########################################################
  sigmoid <- function(x, threshold = 0.5){
    value <- 1.0 / (1.0 + exp(-x))
    
    if(value >= threshold){
      return(1)
    }else if(value < threshold){
      return(0)
    }
  }
  
# LINEAR FUNCTION ###########################################################
  linear <- function(x){
    return(x)
  }

# BINARY STEP FUNCTION ######################################################
  binary_step <- function(x){
    if(x>=0){
      temp_var = 1
    }else if(x<0){
      temp_var = 0
    }
    return(temp_var)
  }
  
# TANH FUNCTION ###########################################################
  tanh <- function(x){
    return(tanh(x))
  }
  
