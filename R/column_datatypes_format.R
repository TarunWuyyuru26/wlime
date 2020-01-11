
# Getting the data types of the columns in a dataframe in a proper format
col_dtypes_format <- function(data, op = 'df'){
  
  # Getting the Column Names of the input data
  df_colnames = colnames(data)
  
  if(op == 'print'){
    for(i in colnames(data)){
      cat(i,'-->', class(data[,i]),"\n")
    }
  }
  else if(op == 'df'){
    
    col_vec = colnames(data)
    class_vec = c()
    
    for(i in 1:length(col_vec)){
      class_vec[i] = class(data[,col_vec[i]])
    }
    
    # Assigning the Values
    temp_df <- data.frame(Column_Name=col_vec, Column_Type=class_vec)
    
    # Returning the Dataframe
    return(temp_df)
  }
}
