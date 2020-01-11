
# Data Type Extractor
dtype_extractor <- function(ipdata, data_type = "numeric"){
  if(data_type == "integer"){
    ipdata = ipdata[, unlist(lapply(ipdata, is.integer))]
  }else if(data_type == "numeric"){
    ipdata = ipdata[, unlist(lapply(ipdata, is.numeric))]
  }else if(data_type == "factor"){
    ipdata = ipdata[, unlist(lapply(ipdata, is.factor))]
  }else if(data_type == "character"){
    ipdata = ipdata[, unlist(lapply(ipdata, is.character))]
  }else{
    print("Invalid Input to 'data_type'")
  }
  return(ipdata)
}