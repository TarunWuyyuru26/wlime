
# Finding out Correlation between Categorical Variables using chi.Squared Test
cat_corr <- function(catData, to_Factor = FALSE, op_Mat_or_Plot = "matrix"){
  
  # Preprocessing the Given Data
  if(to_Factor == FALSE){
    catData
  }else if(to_Factor == TRUE){
    cols = colnames(catData)
    catData[cols] = lapply(catData[cols], as.factor)
  }else{
    print("Invalid input to 'to_Factor' Parameter")
  }
  
  # Generating the Correlation Matrix
  emp_mat = matrix(nrow = ncol(catData),ncol = ncol(catData))
  colnames(emp_mat) = rownames(emp_mat) = colnames(catData)
  
  for (i in (colnames(catData))){
    for (j in (colnames(catData))){
      cont_table = table(i = catData[,i], j = catData[,j])
      suppressWarnings({
        chi2 = chisq.test(cont_table, correct = FALSE)
      })
      emp_mat[i,j] = chi2$p.value
    }
  }
  
  # Returning the Required Object
  if(op_Mat_or_Plot == "matrix"){
    return(emp_mat)
  }else if(op_Mat_or_Plot == "plot"){
    return(corrplot::corrplot(emp_mat, method = "number", col = "black", cl.pos = "n"))
  }else{
    print("Invalid input to the 'op_Mat_or_Plot' Parameter")
  }
}