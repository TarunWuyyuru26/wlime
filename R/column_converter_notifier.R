
# Tells user which columns he need to convert to get desired data

data_conv_teller <- function(ipdata, num_Attr = "NULL", cat_Attr = "NULL", 
                             char_Attr = "NULL", date_Attr = "NULL", time_Attr = "NULL"){
  
  ###################################################################
  
  # Getting the Datatypes of the Columns
  dtypes = sapply(ipdata, class)
  
  # Converting the Integer columns into Numeric Type for easy search
  int_pos = which(dtypes %in% "integer")
  dtypes[int_pos] <- "numeric"
  
  # Creating a Vector of certain datatype for matching later
  suppressWarnings(
    if(num_Attr == "NULL"){
      num_vec <- NULL
    }else{
      num_vec <- vector(mode="character", length= length(num_Attr))
      names(num_vec) <- num_Attr
      num_vec = replace(x = num_vec, list = seq(1:length(num_vec)), values = "numeric")
    }
  )
  
  suppressWarnings(
    if(cat_Attr == "NULL"){
      cat_vec <- NULL
    }else{
      cat_vec <- vector(mode="character", length= length(cat_Attr))
      names(cat_vec) <- cat_Attr
      cat_vec = replace(x = cat_vec, list = seq(1:length(cat_vec)), values = "factor")
    }
  )
  
  suppressWarnings(
    if(char_Attr == "NULL"){
      char_vec <- NULL
    }else{
      char_vec <- vector(mode="character", length= length(cat_Attr))
      names(char_vec) <- char_Attr
      char_vec = replace(x = char_vec, list = seq(1:length(char_vec)), values = "character")
    }
  )
  
  suppressWarnings(
    if(date_Attr == "NULL"){
      date_vec <- NULL
    }else{
      date_vec <- vector(mode="character", length= length(date_Attr))
      names(date_vec) <- date_Attr
      date_vec = replace(x = date_vec, list = seq(1:length(date_vec)), values = "date")
    }
  )
  
  suppressWarnings(
    if(time_Attr == "NULL"){
      time_vec <- NULL
    }else{
      time_vec <- vector(mode="character", length= length(time_Attr))
      names(time_vec) <- time_Attr
      time_vec = replace(x = time_vec, list = seq(1:length(time_vec)), values = "time")
    }
  )
  
  # Combining the output of all vectors
  formed_vec <- c(cat_vec, num_vec, char_vec, date_vec, time_vec)
  formed_vec = formed_vec[names(dtypes)]
  
  # Mismatched Column Positions
  mis_pos = which((dtypes == formed_vec) %in% FALSE)
  
  # Prinitng the Final Outcome
  for(i in mis_pos){
    cat(names(formed_vec[i]), ":", dtypes[i], "-->", formed_vec[i], "\n")
  }
}
