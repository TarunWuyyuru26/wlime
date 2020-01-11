
# Data Type Converter
dtype_converter <- function(ipdata, columns, convert_to = "numeric"){
  if(convert_to == "integer"){
    ipdata[,columns] = lapply(ipdata[,columns], as.integer)
  }else if(convert_to == "numeric"){
    ipdata[,columns] = lapply(ipdata[,columns], as.numeric)
  }else if(convert_to == "factor"){
    ipdata[,columns] = lapply(ipdata[,columns], as.factor)
  }else if(convert_to == "character"){
    ipdata[,columns] = lapply(ipdata[,columns], as.character)
  }else{
    print("Invalid Input to 'convert_to'")
  }
  return(ipdata)
}