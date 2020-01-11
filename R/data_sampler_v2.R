
# Version_2
data_sampler_v2 <- function(data, target, train_size = 70, val_size = 20,
                            test_size = 10, stratify = FALSE){
  
  # Main Process
  ratios_total = train_size + val_size + test_size
  
  if(ratios_total != 100){
    print("summation of the ratios is not equal to 100")
  }else if(ratios_total == 100){
    if(stratify == FALSE){
      
      # Initial Math
      modified_train_size = (train_size/100)
      modified_val_size = (val_size/100)
      modified_test_size = (test_size/100)
      
      val_test_total = modified_val_size + modified_test_size
      process_val_size = (modified_val_size/val_test_total)
      process_test_size = (modified_test_size/val_test_total)
      
      #########################################################
      sample_seq = 1:length(target)
      
      train_rows <- sample(x = 1:length(sample_seq), 
                           size = modified_train_size*length(sample_seq),replace = FALSE)
      
      train_idxs = sample_seq[train_rows]
      val_test_rows = sample_seq[-train_rows]
      
      ########################################################
      
      test_rows <- sample(x = 1:length(val_test_rows), 
                          size = process_test_size*length(val_test_rows),replace = FALSE)
      
      if(length(test_rows) == 0){
        val_idxs = val_test_rows
        test_idxs = 0
      }else if(length(test_rows) != 0){
        val_idxs = val_test_rows[-test_rows]
        test_idxs = val_test_rows[test_rows]
      }else{
        print("Some other problem !!!")
      }
      
      return(list("train_data" = data[train_idxs,],
                  "val_data" = data[val_idxs,],
                  "test_data" = data[test_idxs,]))
      
    }else if(stratify == TRUE){
      print("under construction !!!, will be here soon")
    }
  }
}