
# REGRESSION METRICS

  # MSE ######################################################################

    mse <- function(y_trues, y_preds){
      
      if(length(y_preds) != length(y_trues)){
        stop(paste("Length of the y_trues & y_preds must be same"))
      }
      
      ((sum((y_trues - y_preds)^2))/length(y_trues))
    }


  # MAE ######################################################################

    mae <- function(y_trues, y_preds){
      
      if(length(y_preds) != length(y_trues)){
        stop(paste("Length of the y_trues & y_preds must be same"))
      }
      
      ((sum(abs(y_trues - y_preds)))/length(y_trues))
    }


  # MAPE ######################################################################

    mape <- function(y_trues, y_preds){
      
      if(length(y_preds) != length(y_trues)){
        stop(paste("Length of the y_trues & y_preds must be same"))
      }
      
      ((sum(abs(y_trues - y_preds)))/length(y_trues))*100
    }


  # RMSE ######################################################################

    rmse <- function(y_trues, y_preds){
      
      if(length(y_preds) != length(y_trues)){
        stop(paste("Length of the y_trues & y_preds must be same"))
      }
      
      sqrt((sum((y_trues - y_preds)^2))/length(y_trues))
    }


  # MPE ######################################################################

    mpe <- function(y_trues, y_preds){
      
      if(length(y_preds) != length(y_trues)){
        stop(paste("Length of the y_trues & y_preds must be same"))
      }
      
      ((sum(y_trues - y_preds))/length(y_trues))*100
    }
    