
# Dataframe Seperator of Numeric & Nonnumeric Columns
df_diffrentiator <- function(ipdata){
  
  logical_op = sapply(ipdata, is.numeric)
  num_cols = which(logical_op == TRUE)
  non_num_cols = which(logical_op != TRUE)
  return(list("numdata" = num_cols, "non_numdata" = non_num_cols))
}