
# MAKING THE ORDER OF COLUMNS AS THE SAME ORDER IN THE TRAIN ##############################

# TYPE-1 ------------------------------------------------------------------

df_col_order_maker_V1 <- function(ipdata){
  train_cols = colnames(ipdata)
  class(train_cols) = 'trainDF_colOrder_V1'
  return(train_cols)
}

predict.trainDF_colOrder_V1 <- function(object, testdata, remove_unseen_cols = FALSE){
  
  if(remove_unseen_cols == TRUE){
    
    final_test_cols = test_cols_order[test_cols_order %in% train_cols_order]
    return(final_test_cols)
    
  }else if(remove_unseen_cols == FALSE){
    
    check_tr_te = train_cols_order %in% test_cols_order
    
    if(all(check_tr_te) == FALSE){
      stop(paste("Some Columns of train are missing in test"))
    }
    
    unseen_cols = test_cols_order[!test_cols_order %in% train_cols_order]
    final_test_cols = c(train_cols_order,unseen_cols)
    return(final_test_cols)
    
  }
}