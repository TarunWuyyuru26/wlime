
# CLASSIFICATION METRICS

  # CONFUSION MATRIX #########################################################
    
    confusion_Matrix <- function(y_preds, y_trues, verbose = FALSE, pos_class = 'none'){
      
      # Checking if both the inputs are factors
      if(class(y_trues) != "factor"){
        stop(paste("y_trues is not of factor type"))
      }
      
      if(class(y_preds) != "factor"){
        stop(paste("y_preds is not of factor type"))
      }
      
      # Checking if the number of levels in trues and preds are same or not
      unq_lvls_trues = unique(as.character(y_trues))
      unq_lvls_preds = unique(as.character(y_preds))
      
      if(all(unq_lvls_trues == unq_lvls_preds)){
        if(verbose == TRUE){
          print("y_trues and y_preds have same levels")
        }
      }else{
        stop(paste("Levels in y_trues and y_preds are not matching !!!"))
      }
      
      # Initiating 2 different processes based on number of levels
      
      y_trues_unique = length(unique(y_trues))
      y_preds_unique = length(unique(y_preds))
      
      if((y_trues_unique <= 2) & (y_preds_unique <= 2)){
        
        if(verbose == TRUE){
          cat("  Binary Confusion Matrix","\n")
        }
        
        # creating basic Confusion Matrix
        
        conf_mat = table(y_preds, y_trues)
        conf_mat
        
        # Getting Positive and Negative Classes
        if(pos_class == 'none'){
          pos_class = colnames(conf_mat)[1]
          neg_class = colnames(conf_mat)[2]
        }else if(pos_class != 'none'){
          neg_class = colnames(conf_mat)[!colnames(conf_mat) %in% pos_class]
          neg_class
        }else{
          pos_class = colnames(conf_mat)[1]
          neg_class = colnames(conf_mat)[2]
        }
        
        # Rearrange the Confusion Matrix if Positive Class gets changed
        if(pos_class != 'none'){
          order_of_labels = c(pos_class, neg_class)
          conf_mat = conf_mat[order_of_labels,order_of_labels]
        }
        
        # Calculating the metrics
        tp = conf_mat[1,1]
        tn = conf_mat[2,2]
        fp = conf_mat[1,2]
        fn = conf_mat[2,1]
        
        accuracy = sum(diag(conf_mat))/(sum(conf_mat))
        precision = tp/(tp+fp)
        recall = tp/(tp+fn)
        specificity = tn/(tn+fp)
        f1_score = (2*precision*recall)/(precision+recall)
        
        # Printing the Output
        print(conf_mat)
        # cat("\n")
        cat("\n","----------------","\n")
        
        cat("\n","Positive Class :", pos_class)
        cat("\n","True Positive :", tp)
        cat("\n","False Positive :", fp)
        cat("\n","False Negative :", fn)
        cat("\n","True Negative :", tn)
        
        cat("\n","\n","----------------","\n")
        
        cat("\n","accuracy :", accuracy)
        cat("\n","precision :", precision)
        cat("\n","recall :", recall)
        cat("\n","specificity :", specificity)
        cat("\n","f1_score :", f1_score)
        
        cat("\n","\n")
        
        # Returning the Object
        
        return(list("ConfusionMatrix" = conf_mat,
                    "basic_metrics" = list("truePositive" = tp,
                                           "falsePositive" = fp,
                                           "falseNegative" = fn,
                                           "trueNegative" = tn,
                                           "positive_class" = pos_class),
                    "metrics" = list("accuracy" = accuracy,
                                     "precision" = precision,
                                     "recall" = recall,
                                     "specificity" = specificity,
                                     "f1_score" = f1_score)))
        
      }else if((y_trues_unique >= 2) & (y_preds_unique >= 2)){
        
        cat("  MultiClass Confusion Matrix","\n")
        
        conf_mat = table(y_preds, y_trues)
        print(conf_mat)
        
        cat("\n","accuracy :", accuracy, "\n")
        
        empty_list = list("basics" = list("TruePositives" = list(),
                                          "FalsePositives" = list(),
                                          "FalseNegatives" = list(),
                                          "TrueNegatives" = list()),
                          "metrics" = list("Accuracy" = (sum(diag(conf_mat)))/(sum(conf_mat)),
                                           "Precision" = list(),
                                           "Recall" = list(),
                                           "Specificity" = list(),
                                           "f1_Score" = list()),
                          "AveragedMetrics" = list())
        
        for (i in 1:length(unq_lvls_trues)) {
          
          # get index of the level
          col_level = unq_lvls_trues[i]
          idx = i
          
          # Basic Metrics
          tp = conf_mat[i,i]
          fp = sum(conf_mat[i,-i])
          fn = sum(conf_mat[-i,i])
          tn = sum(conf_mat[-i,-i])
          
          # Evaluation Metrics
          precision = tp/(tp+fp)
          recall = tp/(tp+fn)
          specificity = tn/(tn+fp)
          f1_score = (2*precision*recall)/(precision+recall)
          
          # Printing the Output
          cat("\n","==============================","\n")
          
          cat("\n","Class :", unq_lvls_trues[i])
          cat("\n","True Positive :", tp)
          cat("\n","False Positive :", fp)
          cat("\n","False Negative :", fn)
          cat("\n","True Negative :", tn)
          
          cat("\n","\n","----------------","\n")
          
          cat("\n","precision :", precision)
          cat("\n","recall :", recall)
          cat("\n","specificity :", specificity)
          cat("\n","f1_score :", f1_score)
          
          cat("\n","\n")
          
          # Assigning Basic Metrics to the empty list (per class)
          empty_list[['basics']][['TruePositives']][unq_lvls_trues[i]] = tp
          empty_list[['basics']][['FalsePositives']][unq_lvls_trues[i]] = fp
          empty_list[['basics']][['FalseNegatives']][unq_lvls_trues[i]] = fn
          empty_list[['basics']][['TrueNegatives']][unq_lvls_trues[i]] = tn
          
          # Assigning Evaluation Metrics to the empty list (per class)
          empty_list[['metrics']][['Precision']][unq_lvls_trues[i]] = precision
          empty_list[['metrics']][['Recall']][unq_lvls_trues[i]] = recall
          empty_list[['metrics']][['Specificity']][unq_lvls_trues[i]] = specificity
          empty_list[['metrics']][['f1_Score']][unq_lvls_trues[i]] = f1_score
        }
        
        # Assigning the average of the evaluation metrics
        
        for (j in names(empty_list$metrics)) {
          if(j != 'Accuracy'){
            
            sum = 0
            len = 0
            
            for(k in empty_list[['metrics']][[j]]){
              sum = sum + k
              len = len + 1
            }
            
            average_value = sum/len
            
            empty_list[['AveragedMetrics']][[paste0('average',j)]] = average_value
          }
        }
        
        # Printing the Averaged Values
        
        cat("\n","==============================","\n")
        
        for (l in names(empty_list$AveragedMetrics)) {
          cat("\n",l, ":",empty_list[['AveragedMetrics']][[l]])
        }
        
        cat("\n","\n")
        
        # Returning the final object
        
        return(list("ConfusionMatrix" = conf_mat,
                    "Basics" = empty_list$basics,
                    "Individual_Metrics" = empty_list$metrics,
                    "Averaged_Metrics" = list("precision" = empty_list$AveragedMetrics$averagePrecision,
                                              "recall" = empty_list$AveragedMetrics$averageRecall,
                                              "specificity" = empty_list$AveragedMetrics$averageSpecificity,
                                              "f1_score" = empty_list$AveragedMetrics$averagef1_Score)))
      }
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    