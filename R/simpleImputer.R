
# Function for getting the meta data of the dataframe

simpleImpute <- function(ipdata, Num_entity = "mean"){
  
  # Multi Mode Function
  multi_mode <- function(ipvec, multimodal_op = FALSE, verbose = FALSE){
    
    freq_of_levels = table(ipvec)
    maxfreq_count = max(freq_of_levels)
    
    freq_of_levels = ifelse(freq_of_levels == maxfreq_count, TRUE, FALSE)
    
    multimodal_instances = rownames(which(x = freq_of_levels, arr.ind = TRUE))
    count_of_multimodal_instances = length(multimodal_instances)
    
    if(verbose)
      cat("No. of Instances for Mode is :", count_of_multimodal_instances, "\n")
    
    if(multimodal_op == TRUE){
      FinalMode = multimodal_instances
      
      if(verbose)
        cat("Unique Mode/s :", FinalMode, "\n")
      
      return(FinalMode)
      
    }else if(multimodal_op == FALSE){
      rndm_index = sample(x = 1:length(multimodal_instances), size = 1)
      FinalMode = multimodal_instances[rndm_index]
      
      if(verbose)
        cat("Multi Mode/s :", FinalMode, "\n")
      
      return(FinalMode)
    }
  }
  
  # Process
  logical_op = sapply(ipdata, is.numeric)
  num_cols = which(logical_op == TRUE)
  non_num_cols = which(logical_op != TRUE)
  
  num_df = ipdata[,names(num_cols)]
  non_num_df = ipdata[,names(non_num_cols)]
  
  if(Num_entity == "mean"){
    num_cols_data = lapply(na.omit(num_df), mean)
    non_num_data = lapply(na.omit(non_num_df), multi_mode)
  }else if(Num_entity == "median"){
    num_cols_data = lapply(na.omit(num_df), median)
    non_num_data = lapply(na.omit(non_num_df), multi_mode)
  }
  
  # Returning the Final Object
  metadata_object = list("numdata" = num_cols_data, "non_numdata" = non_num_data)
  class(metadata_object) = 'simpleImpute'
  return(metadata_object)
}

# Imputing New Data with the Data Object of Train Data

predict.simpleImpute <- function(object, newdata){ 
  
  # Checking if atleast one NA is present in a Column or a Vector or complete DF
  atlstOneNA <- function(ip){
    NA_logical_Op = is.na(ip)
    if((TRUE %in% NA_logical_Op) == TRUE){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  # Imputing Process
  for (col in colnames(newdata)){
    if(atlstOneNA(newdata[col]) == TRUE){
      if((class(newdata[,col]) == "numeric") | (class(newdata[,col]) == "integer")){
        NA_logical = match(x = newdata[,col], table = NA)
        NA_locs = which(NA_logical == 1)
        newdata[NA_locs,col] <- object$numdata[[col]]
      }else if((class(newdata[,col]) != "numeric") | (class(newdata[,col]) != "integer")){
        NA_logical = match(x = newdata[,col], table = NA)
        NA_locs = which(NA_logical == 1)
        newdata[NA_locs,col] <- object$non_numdata[[col]]
      }
    }else{
      next
    }
  }
  
  # Returning the Final Imputed Dataframe
  return(newdata)
}
