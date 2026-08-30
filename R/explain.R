#' Explain a Fitted DairyPredictModel
#'
#' Calculate variable importance or model coefficients
#' for a fitted DairyPredictR model.
#'
#' @param object A fitted DairyPredictModel object.
#' @return A data.frame containing variable importance.
#' @export

explain <- function(object) {
  
  # ----------------------------------------------------------
  # CHECK MODEL
  # ----------------------------------------------------------
  
  if (!inherits(object, "DairyPredictModel")) {
    
    stop(
      "object must be a DairyPredictModel."
    )
    
  }
  
  
  # ----------------------------------------------------------
  # CHECK PREDICTORS
  # ----------------------------------------------------------
  
  if (is.null(object$predictors) ||
      length(object$predictors) == 0) {
    
    stop(
      "No predictor variables are stored in the model."
    )
    
  }
  
  
  predictors <- object$predictors
  
  
  # ----------------------------------------------------------
  # LINEAR REGRESSION
  # ----------------------------------------------------------
  
  if (object$method == "linear") {
    
    coefficients <- stats::coef(
      object$model
    )
    
    coefficients <- coefficients[
      names(coefficients) %in% predictors
    ]
    
    result <- data.frame(
      
      Variable = names(coefficients),
      
      Importance = abs(
        as.numeric(coefficients)
      ),
      
      Direction = ifelse(
        coefficients >= 0,
        "Positive",
        "Negative"
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # RANDOM FOREST
  # ----------------------------------------------------------
  
  else if (object$method == "rf") {
    
    importance_values <- randomForest::importance(
      object$model
    )
    
    importance_values <- importance_values[
      predictors,
      ,
      drop = FALSE
    ]
    
    if ("IncNodePurity" %in%
        colnames(importance_values)) {
      
      importance <- importance_values[
        ,
        "IncNodePurity"
      ]
      
    } else {
      
      importance <- importance_values[
        ,
        1
      ]
      
    }
    
    result <- data.frame(
      
      Variable = rownames(
        importance_values
      ),
      
      Importance = as.numeric(
        importance
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # GRADIENT BOOSTING MACHINE
  # ----------------------------------------------------------
  
  else if (object$method == "gbm") {
    
    importance_values <-
      gbm::summary.gbm(
        object$model,
        plotit = FALSE
      )
    
    importance_values <-
      importance_values[
        importance_values$var %in% predictors,
        ,
        drop = FALSE
      ]
    
    result <- data.frame(
      
      Variable = importance_values$var,
      
      Importance = importance_values$rel.inf,
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # SUPPORT VECTOR MACHINE
  # ----------------------------------------------------------
  
  else if (object$method == "svm") {
    
    ## Training data
    svm_data <- object$data_used
    
    ## Check training data
    if (!is.data.frame(svm_data)) {
      
      stop(
        "Training data are not available for SVM explainability."
      )
      
    }
    
    ## Keep only target + predictors
    svm_data <- svm_data[
      ,
      c(object$target, predictors),
      drop = FALSE
    ]
    
    ## Observed target
    actual <- svm_data[[object$target]]
    
    ## Baseline prediction
    baseline_prediction <- stats::predict(
      object$model,
      newdata = svm_data[, predictors, drop = FALSE]
    )
    
    ## Baseline RMSE
    baseline_rmse <- sqrt(
      mean(
        (actual - baseline_prediction)^2,
        na.rm = TRUE
      )
    )
    
    ## Importance container
    importance_values <- numeric(
      length(predictors)
    )
    
    names(importance_values) <- predictors
    
    ## Reproducibility
    set.seed(123)
    
    ## Permutation importance
    for (variable in predictors) {
      
      modified_data <- svm_data[
        ,
        predictors,
        drop = FALSE
      ]
      
      ## Permute one predictor
      modified_data[[variable]] <- sample(
        modified_data[[variable]]
      )
      
      ## Prediction after permutation
      modified_prediction <- stats::predict(
        object$model,
        newdata = modified_data
      )
      
      ## New RMSE
      modified_rmse <- sqrt(
        mean(
          (
            actual -
              modified_prediction
          )^2,
          na.rm = TRUE
        )
      )
      
      ## Increase in RMSE
      importance_values[variable] <-
        modified_rmse -
        baseline_rmse
    }
    
    ## Result
    result <- data.frame(
      
      Variable = predictors,
      
      Importance = as.numeric(
        importance_values
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # ARTIFICIAL NEURAL NETWORK
  # ----------------------------------------------------------
  
  else if (object$method == "ann") {
    
    ## Generalized weights
    generalized_weights <-
      object$model$generalized.weights[[1]]
    
    ## Check dimensions
    if (is.null(generalized_weights) ||
        !is.matrix(generalized_weights)) {
      
      stop(
        "Generalized weights are not available for ANN model."
      )
      
    }
    
    ## Calculate mean absolute generalized weight
    importance_values <- colMeans(
      abs(generalized_weights),
      na.rm = TRUE
    )
    
    ## Make sure number of values matches predictors
    if (length(importance_values) != length(predictors)) {
      
      stop(
        paste0(
          "ANN generalized weights contain ",
          length(importance_values),
          " variables, but ",
          length(predictors),
          " predictors were supplied."
        )
      )
      
    }
    
    ## Assign predictor names explicitly
    names(importance_values) <- predictors
    
    ## Create result
    result <- data.frame(
      
      Variable = predictors,
      
      Importance = as.numeric(
        importance_values
      ),
      
      stringsAsFactors = FALSE
      
    )
    
  }
  
  
  # ----------------------------------------------------------
  # UNSUPPORTED MODEL
  # ----------------------------------------------------------
  
  else {
    
    stop(
      paste0(
        "Explainability is not implemented for model: ",
        object$method
      )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # SORT IMPORTANCE
  # ----------------------------------------------------------
  
  result <- result[
    order(
      result$Importance,
      decreasing = TRUE
    ),
    ,
    drop = FALSE
  ]
  
  
  rownames(result) <- NULL
  
  
  # ----------------------------------------------------------
  # RETURN
  # ----------------------------------------------------------
  
  return(result)
  
}