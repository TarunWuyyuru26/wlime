
# Getting the data types of the columns in a dataframe
col_dtypes <- function(ipdata){
  return(sapply(ipdata, class))
}
