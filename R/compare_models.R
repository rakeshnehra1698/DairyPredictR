#' Compare Multiple Machine Learning Models
#'
#' Train and evaluate multiple machine learning algorithms.
#'
#' @param data A list returned by split_data().
#' @param target Character. Target variable.
#' @param predictors Character vector of predictor variables.
#' @param identifier Character. Identifier variable.
#' @param methods Character vector of algorithms.
#' @param digits Number of decimal places.
#'
#' @return A data.frame of model performance.
#'
#' @export

compare_models <- function(
    data,
    target,
    predictors,
    identifier = NULL,
    methods = c("linear", "rf", "gbm", "svm", "ann"),
    digits = 4
) {
  
  ## ----------------------------------------------------------
  ## Check split object
  ## ----------------------------------------------------------
  
  if (!is.list(data)) {
    stop("data must be the output of split_data().")
  }
  
  if (is.null(data$train) || is.null(data$test)) {
    stop("data must contain train and test datasets.")
  }
  
  
  ## ----------------------------------------------------------
  ## Extract training and testing data
  ## ----------------------------------------------------------
  
  train <- data$train
  test  <- data$test
  
  
  ## ----------------------------------------------------------
  ## Check variables
  ## ----------------------------------------------------------
  
  if (!(target %in% names(train))) {
    stop("Target variable not found in training data.")
  }
  
  missing_predictors <- setdiff(predictors, names(train))
  
  if (length(missing_predictors) > 0) {
    stop(
      paste(
        "Predictor(s) not found in training data:",
        paste(missing_predictors, collapse = ", ")
      )
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Results container
  ## ----------------------------------------------------------
  
  results <- data.frame()
  
  
  ## ----------------------------------------------------------
  ## Train and evaluate each model
  ## ----------------------------------------------------------
  
  for (method in methods) {
    
    result <- tryCatch({
      
      ## ----------------------------------------------
      ## Fit model
      ## ----------------------------------------------
      
      model <- fit_model(
        data = train,
        target = target,
        predictors = predictors,
        identifier = identifier,
        method = method
      )
      
      
      ## ----------------------------------------------
      ## Evaluate model
      ## ----------------------------------------------
      
      metrics <- evaluate(
        object = model,
        newdata = test,
        verbose = FALSE
      )
      
      
      ## ----------------------------------------------
      ## Create result row
      ## ----------------------------------------------
      
      row <- data.frame(
        Model = switch(
          method,
          linear = "Linear Regression",
          rf     = "Random Forest",
          gbm    = "Gradient Boosting Machine",
          svm    = "Support Vector Machine",
          ann    = "Artificial Neural Network",
          toupper(method)
        ),
        R2 = NA_real_,
        RMSE = NA_real_,
        MAE = NA_real_,
        MAPE = NA_real_,
        Bias = NA_real_,
        Error = ""
      )
      
      
      ## ----------------------------------------------
      ## Insert metrics
      ## ----------------------------------------------
      
      for (i in seq_len(nrow(metrics))) {
        
        metric_name <- metrics$Metric[i]
        
        if (metric_name %in% names(row)) {
          
          row[[metric_name]] <- metrics$Value[i]
          
        }
        
      }
      
      
      row
      
    }, error = function(e) {
      
      ## ----------------------------------------------
      ## Keep failed model in comparison table
      ## ----------------------------------------------
      
      data.frame(
        Model = switch(
          method,
          linear = "Linear Regression",
          rf     = "Random Forest",
          gbm    = "Gradient Boosting Machine",
          svm    = "Support Vector Machine",
          ann    = "Artificial Neural Network",
          toupper(method)
        ),
        R2 = NA_real_,
        RMSE = NA_real_,
        MAE = NA_real_,
        MAPE = NA_real_,
        Bias = NA_real_,
        Error = conditionMessage(e)
      )
      
    })
    
    
    ## Add result
    
    results <- rbind(
      results,
      result
    )
    
  }
  
  
  ## ----------------------------------------------------------
  ## Round numeric values
  ## ----------------------------------------------------------
  
  numeric_cols <- sapply(
    results,
    is.numeric
  )
  
  results[numeric_cols] <- lapply(
    results[numeric_cols],
    round,
    digits = digits
  )
  
  
  ## ----------------------------------------------------------
  ## Return results
  ## ----------------------------------------------------------
  
  results
  
}