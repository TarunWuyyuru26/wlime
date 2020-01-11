
# SOFTMAX FUNCTION ###########################################################
  softmax <- function(n_classes = 1){
    
    weights_for_classes = matrix(data = vector(length = n_classes), 
                                 nrow = 1, ncol = n_classes, 
                                 byrow = TRUE)
    return(weights_for_classes)
    
  }

# RELU FAMILY ###########################################################
  
  # ReLU --------------------------------------------------
    ReLU <- function(x){
      if(x > 0){
        return(x)
      }else if(x <= 0){
        return(0)
      }
    }
  
  # Leaky-ReLU --------------------------------------------------
    leakyReLU <- function(x){
      if(x > 0){
        return(x)
      }else if(x <= 0){
        return(0.01*x)
      }
    }
    
  # Parameteric-ReLU --------------------------------------
    pReLU <- function(x, alpha = 0.1){
      if(x>0){
        return(x)
      }else{
        return(x*alpha)
      }
    }
  
  # Randomized-Leaky-ReLU --------------------------------------
    RReLU <- function(x){
      
      random_value <- sample(x = -3:3, size = 1)
      
      if(x>0){
        return(x)
      }else{
        return(x*random_value)
      }
    }  
  
  # Exponential Linear Unit --------------------------------------
    ELU <- function(x, alpha = 0.01){
      if(x>0){
        return(x)
      }else{
        return(alpha(exp(x)-1))
      }
    }

  
  
  
  
  
  
      
  
  

    
