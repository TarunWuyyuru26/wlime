
# Removing the Rows which have Missing values in the Target
targ_purifier <- function(ipData, targ_col, verbose = TRUE){
  missVal_Logical = match(x = targ_col, table = NA)
  unwanted_rows = which(missVal_Logical == 1)
  ipData = ipData[-unwanted_rows,]
  
  if(verbose == TRUE){
    # Printing Results
    cat("Number of Rows[Omitted] : ",length(unwanted_rows), "\n")
    cat("Percentage of Rows[Omitted] : ",((length(unwanted_rows)/nrow(ipData))*100), "%")
  }
  
  # Returning the Final DF
  return(ipData)
}