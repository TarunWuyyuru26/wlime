
# rm(list = ls(all = TRUE))

# data = iris
# View(data)


num_data_trsfrm <- function(ipdata, method = 'standardize'){
  
  if(method == 'standardize'){
    
    means = sapply(X = data, FUN = function(x){
      if(class(x) == 'numeric'){
        return(mean(x = x, na.rm = TRUE))
      }else{
        return(NULL)
      }
    })
    
    std_dev = sapply(X = data, FUN = function(x){
      if(class(x) == 'numeric'){
        return(sd(x = x, na.rm = TRUE))
      }else{
        return(NULL)
      }
    })
    
    # Returning the object
    metadata_object = list('mean_values' = means,'std_devs' = std_dev)
    class(metadata_object) = 'standardize_normalize'
    return(metadata_object)
    
  }else if(method == 'normalize'){
    
    # normalization #######################################
    
    maxes = sapply(X = data, FUN = function(x){
      if(class(x) == 'numeric'){
        return(max(x))
      }else{
        return(NULL)
      }
    })
    
    mins = sapply(X = data, FUN = function(x){
      if(class(x) == 'numeric'){
        return(min(x))
      }else{
        return(NULL)
      }
    })
    
    # Returning the object  
    metadata_object = list('max_values' = maxes, 'min_values' = mins)
    class(metadata_object) = 'standardize_normalize'
    return(metadata_object)
  }
}

predict.standardize_normalize <- function(object, data){
  
  # Process
  if(names(object[1]) == 'mean_values'){
    
    # Standarization
    for (i in 1:ncol(data)) {
      if(class(data[[i]]) == 'numeric'){
        data[[i]] = (data[[i]] - object$mean_values[[i]])/(object$std_devs[[i]])
      }
    }
    
  }else if(names(object[1]) == 'max_values'){
    
    # Normalization
    for (i in 1:ncol(data)) {
      if(class(data[[i]]) == 'numeric'){
        data[[i]] = (data[[i]] - object$min_values[[i]])/(object$max_values[[i]] - object$min_values[[i]])
      }
    }
    
  }
  
  # Returning the final object
  return(data)
}

# std_obj = num_data_trsfrm(ipdata = iris)
# nrm_obj = num_data_trsfrm(ipdata = iris, method = 'normalize')
# 
# std_obj
# nrm_obj
# 
# std_data = predict(object = std_obj, data = data)
# nrm_data = predict(object = nrm_obj, data = data)
# View(std_data)
# View(nrm_data)














