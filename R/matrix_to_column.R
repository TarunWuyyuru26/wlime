
# The Same above operation for the Matrix to Column [Undummification if a Matrix is given]
matrix_to_col <- function(ipData){
  unDummify_modified <- function(ipData, output_as = "numeric"){
    if(output_as == "numeric"){
      a = as.numeric(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
    }else if(output_as == "factor"){
      a = as.factor(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
    }else if(output_as == "character"){
      a = as.character(apply(ipData,1,function(x){return(names(ipData)[which.max(x)])}))
    }
    return(a)
  }
  
  if(class(ipData) == "matrix"){
    ipData = data.frame(ipData)
    colnames(ipData) = seq(from = 0,to = ((ncol(ipData))-1),by = 1)
    
    z = unDummify_modified(ipData = ipData, output_as = "character")
    z = as.factor(array(z))
    return(z)
  }
}