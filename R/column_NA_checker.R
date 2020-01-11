
# Checking if atleast one NA is present in a Column or a Vector or complete DF
atlstOneNA <- function(ip){
  NA_logical_Op = is.na(ip)
  if((TRUE %in% NA_logical_Op) == TRUE){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

