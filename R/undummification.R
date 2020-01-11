
# UnDummify Some set of Columns [Works only, when some columns given and a new column to be created. (Multiple columns Not Supported)]
unDummify <- function(ipData, column_Name, output_as = "numeric"){
  if(output_as == "numeric"){
    ipData[,column_Name] = as.numeric(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
  }else if(output_as == "factor"){
    ipData[,column_Name] = as.factor(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
  }else if(output_as == "character"){
    ipData[,column_Name] = as.character(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
  }
  return(a)
}