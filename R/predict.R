#' Predict Using a DairyPredictModel
#'
#' Generate predictions from a fitted DairyPredictR model.
#'
#' @param object A DairyPredictModel object.
#' @param newdata A data.frame containing predictor variables.
#' @param ... Additional arguments.
#'
#' @return A numeric vector of predicted values.
#'
#' @export
#' @method predict DairyPredictModel

predict.DairyPredictModel <- function(
    object,
    newdata,
    ...
) {
  
  ## ----------------------------------------------------------
  ## Check model
  ## ----------------------------------------------------------
  
  if (!inherits(object, "DairyPredictModel")) {
    
    stop(
      "object must be a DairyPredictModel."
    )
    
  }
  
  
  ## ----------------------------------------------------------
  ## Check input
  ## ----------------------------------------------------------
  
  if (!is.data.frame(newdata)) {
    
    stop(
      "newdata must be a data.frame."
    )
    
  }
  
  
  ## ----------------------------------------------------------
  ## Check predictor variables
  ## ----------------------------------------------------------
  
  missing_cols <- setdiff(
    object$predictors,
    names(newdata)
  )
  
  if (length(missing_cols) > 0) {
    
    stop(
      paste(
        "Missing predictor(s):",
        paste(
          missing_cols,
          collapse = ", "
        )
      )
    )
    
  }
  
  
  ## ----------------------------------------------------------
  ## Select ONLY model predictors
  ## ----------------------------------------------------------
  
  predictor_data <- newdata[
    ,
    object$predictors,
    drop = FALSE
  ]
  
  
  ## ----------------------------------------------------------
  ## Generate predictions
  ## ----------------------------------------------------------
  
  pred <- switch(
    
    object$method,
    
    
    ## ========================================================
    ## LINEAR REGRESSION
    ## ========================================================
    
    linear = {
      
      stats::predict(
        object$model,
        newdata = predictor_data
      )
      
    },
    
    
    ## ========================================================
    ## RANDOM FOREST
    ## ========================================================
    
    rf = {
      
      stats::predict(
        object$model,
        newdata = predictor_data
      )
      
    },
    
    
    ## ========================================================
    ## GRADIENT BOOSTING MACHINE
    ## ========================================================
    
    gbm = {
      
      stats::predict(
        object$model,
        newdata = predictor_data,
        n.trees = object$model$n.trees
      )
      
    },
    
    
    ## ========================================================
    ## SUPPORT VECTOR MACHINE
    ## ========================================================
    
    svm = {
      
      stats::predict(
        object$model,
        newdata = predictor_data
      )
      
    },
    
    
    ## ========================================================
    ## ARTIFICIAL NEURAL NETWORK
    ## ========================================================
    
    ann = {
      
      ## ------------------------------------------------------
      ## Check scaling information
      ## ------------------------------------------------------
      
      if (
        is.null(object$predictor_means) ||
        is.null(object$predictor_sds) ||
        is.null(object$target_mean) ||
        is.null(object$target_sd)
      ) {
        
        stop(
          paste0(
            "ANN scaling information is missing from ",
            "the fitted model. Please retrain the ANN."
          )
        )
        
      }
      
      
      ## ------------------------------------------------------
      ## Convert predictors to numeric matrix/data.frame
      ## ------------------------------------------------------
      
      ann_input <- predictor_data
      
      
      ## ------------------------------------------------------
      ## Apply TRAINING scaling parameters
      ## ------------------------------------------------------
      
      ann_scaled <- scale(
        ann_input,
        center = object$predictor_means,
        scale = object$predictor_sds
      )
      
      
      ann_scaled <- as.data.frame(
        ann_scaled
      )
      
      
      ## ------------------------------------------------------
      ## Ensure correct predictor names and order
      ## ------------------------------------------------------
      
      names(ann_scaled) <- object$predictors
      
      
      ## ------------------------------------------------------
      ## ANN prediction
      ## ------------------------------------------------------
      
      prediction <- neuralnet::compute(
        object$model,
        ann_scaled
      )
      
      
      scaled_prediction <- as.numeric(
        prediction$net.result
      )
      
      
      ## ------------------------------------------------------
      ## Convert prediction back to original scale
      ## ------------------------------------------------------
      
      original_prediction <- (
        scaled_prediction *
          object$target_sd
      ) +
        object$target_mean
      
      
      original_prediction
      
    },
    
    
    ## ========================================================
    ## Unsupported model
    ## ========================================================
    
    NULL
    
  )
  
  
  ## ----------------------------------------------------------
  ## Check unsupported model
  ## ----------------------------------------------------------
  
  if (is.null(pred)) {
    
    stop(
      paste0(
        "Prediction method '",
        object$method,
        "' is not implemented."
      )
    )
    
  }
  
  
  ## ----------------------------------------------------------
  ## Return numeric predictions
  ## ----------------------------------------------------------
  
  return(
    as.numeric(
      unname(pred)
    )
  )
  
}