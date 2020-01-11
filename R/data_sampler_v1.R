
# Version_1
data_sampler <- function(data, target, train_size = 70, val_size = 20,
                         test_size = 10, stratify = FALSE){
  
  # Initial Math
  len_data = length(target)
  modified_train_size = (train_size/100)
  modified_val_size = (val_size/100)
  modified_test_size = (test_size/100)
  
  # Main Process
  ratios_total = train_size + val_size + test_size
  
  if(ratios_total != 100){
    print("summation of the ratios is not equal to 100")
  }else if(ratios_total == 100){
    if(stratify == FALSE){
      val_test_total = modified_val_size + modified_test_size
      process_val_size = (modified_val_size/val_test_total)
      process_test_size = (modified_test_size/val_test_total)
      
      train_rows <- sample(x = 1:len_data, size = modified_train_size*len_data)
      
      # train_data <- data[train_rows,]
      temp_data <- data[-train_rows, ]
      
      len_temp_data <- nrow(temp_data)
      
      val_rows <- sample(x = 1:len_temp_data, size = process_test_size*len_temp_data)
      # val_data <- temp_data[-val_rows,]
      # test_data <- temp_data[val_rows,]
      
      return(list("train_data" = data[train_rows,],
                  "val_data" = temp_data[-val_rows,],
                  "test_data" = temp_data[val_rows,]))
    }else if(stratify == TRUE){
      print("under construction !!!, will be here soon")
    }
  }
}