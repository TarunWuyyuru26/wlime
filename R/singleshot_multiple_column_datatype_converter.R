
singleshot_dtype_Converter <- function(data, 
                                       cols_to_num = 'none', 
                                       cols_to_int = 'none',
                                       cols_to_fact = 'none',
                                       cols_to_char = 'none'){
  
  # Data Type converter Functionality
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
  
  # Process
  suppressWarnings(
    if(cols_to_num != 'none'){
      temp_data = dtype_converter(ipdata = data, columns = cols_to_num, convert_to = 'numeric')
    }
  )
  
  suppressWarnings(  
    if(cols_to_int != 'none'){
      temp_data = dtype_converter(ipdata = temp_data, columns = cols_to_int, convert_to = 'integer')
    }
  )
  
  suppressWarnings(
    if(cols_to_fact != 'none'){
      temp_data = dtype_converter(ipdata = temp_data, columns = cols_to_fact, convert_to = 'factor')
    }
  )
  
  suppressWarnings(
    if(cols_to_char != 'none'){
      temp_data = dtype_converter(ipdata = temp_data, columns = cols_to_char, convert_to = 'character')
    }
  )
  
  # Returning the Final Dataframe
  return(temp_data)
}

# data = read.csv("D:/iTarun/Work/Project_Work/Knowledge_Bank/Analytics_Bank/W_Pkgs/R/regression_dataset_full.csv")
# converted_df = singleshot_dtype_Converter(data = data,
#                                           cols_to_num = c('Critic_Score','Critic_Count'),
#                                           cols_to_int = c('NA_Sales','EU_Sales'),
#                                           cols_to_fact = c('User_Score','User_Count'),
#                                           cols_to_char = c('Name','Platform'))