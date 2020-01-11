
# Getting Unique Mode and Multiple Modes from the given vector
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

# Example:
# multi_mode(ipvec = c("A","A","B","B","C"), multimodal_op = FALSE)
