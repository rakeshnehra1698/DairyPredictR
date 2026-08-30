# ============================================================
# GRADIENT BOOSTING MACHINE
# ============================================================

fit_gbm <- function(
    data,
    target,
    predictors = NULL,
    n.trees = 500,
    interaction.depth = 3,
    shrinkage = 0.05,
    n.minobsinnode = 10,
    seed = 123,
    ...
) {
  
  ## ----------------------------------------------------------
  ## Check input
  ## ----------------------------------------------------------
  
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame.")
  }
  
  
  ## ----------------------------------------------------------
  ## Check target
  ## ----------------------------------------------------------
  
  if (!(target %in% names(data))) {
    stop("Target variable not found in data.")
  }
  
  
  ## ----------------------------------------------------------
  ## Determine predictors
  ## ----------------------------------------------------------
  
  if (is.null(predictors)) {
    
    predictors <- setdiff(
      names(data),
      target
    )
    
  } else {
    
    predictors <- unique(predictors)
    
  }
  
  
  ## ----------------------------------------------------------
  ## Check predictors
  ## ----------------------------------------------------------
  
  if (length(predictors) == 0) {
    stop("No predictor variables available.")
  }
  
  
  missing_predictors <- setdiff(
    predictors,
    names(data)
  )
  
  if (length(missing_predictors) > 0) {
    
    stop(
      paste0(
        "Predictor variable(s) not found: ",
        paste(
          missing_predictors,
          collapse = ", "
        )
      )
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Prevent target from being a predictor
  ## ----------------------------------------------------------
  
  if (target %in% predictors) {
    
    stop(
      paste0(
        "Target variable '",
        target,
        "' cannot be used as a predictor."
      )
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Keep ONLY target + predictors
  ## ----------------------------------------------------------
  
  model_data <- data[
    ,
    c(target, predictors),
    drop = FALSE
  ]
  
  
  ## ----------------------------------------------------------
  ## Create formula
  ## ----------------------------------------------------------
  
  formula <- stats::as.formula(
    paste(
      target,
      "~",
      paste(
        predictors,
        collapse = " + "
      )
    )
  )
  
  
  ## ----------------------------------------------------------
  ## Train GBM
  ## ----------------------------------------------------------
  
  set.seed(seed)
  
  model <- gbm::gbm(
    formula = formula,
    data = model_data,
    distribution = "gaussian",
    n.trees = n.trees,
    interaction.depth = interaction.depth,
    shrinkage = shrinkage,
    n.minobsinnode = n.minobsinnode,
    verbose = FALSE,
    ...
  )
  
  
  ## ----------------------------------------------------------
  ## Fitted values
  ## ----------------------------------------------------------
  
  fitted_values <- as.numeric(
    stats::predict(
      model,
      newdata = model_data[, predictors, drop = FALSE],
      n.trees = n.trees
    )
  )
  
  
  residual_values <- model_data[[target]] - fitted_values
  
  
  ## ----------------------------------------------------------
  ## Return model object
  ## ----------------------------------------------------------
  
  return(
    
    list(
      
      method = "gbm",
      
      model = model,
      
      formula = formula,
      
      predictors = predictors,
      
      target = target,
      
      n_train = nrow(model_data),
      
      n_predictors = length(predictors),
      
      fitted = fitted_values,
      
      residuals = residual_values,
      data_used = data,
      
      training_time = Sys.time(),
      
      package = "DairyPredictR",
      
      version = tryCatch(
        as.character(
          utils::packageVersion("DairyPredictR")
        ),
        error = function(e) "development"
      )
      
    )
  )
}