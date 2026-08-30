# ======================================================
# DairyPredictR
#
# Support Vector Machine implementation
#
# Internal function
# ======================================================

#' @keywords internal
#' @noRd
fit_svm <- function(
    data,
    target,
    predictors = NULL,
    kernel = "radial",
    cost = 1,
    gamma = NULL,
    epsilon = 0.1,
    seed = 123
) {
  
  ## ----------------------------------------------------
  ## Check input
  ## ----------------------------------------------------
  
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame.")
  }
  
  
  ## ----------------------------------------------------
  ## Check target
  ## ----------------------------------------------------
  
  if (!(target %in% names(data))) {
    stop("Target variable not found in data.")
  }
  
  
  ## ----------------------------------------------------
  ## Determine predictors
  ## ----------------------------------------------------
  
  if (is.null(predictors)) {
    
    predictors <- setdiff(
      names(data),
      target
    )
    
  } else {
    
    predictors <- unique(predictors)
    
  }
  
  
  ## ----------------------------------------------------
  ## Check predictors
  ## ----------------------------------------------------
  
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
  
  
  ## ----------------------------------------------------
  ## Prevent target from being a predictor
  ## ----------------------------------------------------
  
  if (target %in% predictors) {
    
    stop(
      paste0(
        "Target variable '",
        target,
        "' cannot be used as a predictor."
      )
    )
  }
  
  
  ## ----------------------------------------------------
  ## Keep ONLY target + predictors
  ## ----------------------------------------------------
  
  model_data <- data[
    ,
    c(target, predictors),
    drop = FALSE
  ]
  
  
  ## ----------------------------------------------------
  ## Check numeric predictors
  ## ----------------------------------------------------
  
  for (v in predictors) {
    
    if (!is.numeric(model_data[[v]])) {
      
      stop(
        paste0(
          "SVM predictors must be numeric. ",
          "Variable '",
          v,
          "' is not numeric."
        )
      )
    }
  }
  
  
  ## ----------------------------------------------------
  ## Check numeric target
  ## ----------------------------------------------------
  
  if (!is.numeric(model_data[[target]])) {
    
    stop(
      paste0(
        "SVM target variable '",
        target,
        "' must be numeric."
      )
    )
  }
  
  
  ## ----------------------------------------------------
  ## Default gamma
  ## ----------------------------------------------------
  
  if (is.null(gamma)) {
    
    gamma <- 1 / length(predictors)
    
  }
  
  
  ## ----------------------------------------------------
  ## Create formula
  ## ----------------------------------------------------
  
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
  
  
  ## ----------------------------------------------------
  ## Set seed
  ## ----------------------------------------------------
  
  set.seed(seed)
  
  
  ## ----------------------------------------------------
  ## Train SVM
  ## ----------------------------------------------------
  
  model <- e1071::svm(
    formula = formula,
    data = model_data,
    kernel = kernel,
    cost = cost,
    gamma = gamma,
    epsilon = epsilon,
    type = "eps-regression"
  )
  
  
  ## ----------------------------------------------------
  ## Fitted values
  ## ----------------------------------------------------
  
  fitted_values <- as.numeric(
    stats::predict(
      model,
      newdata = model_data[, predictors, drop = FALSE]
    )
  )
  
  
  ## ----------------------------------------------------
  ## Residuals
  ## ----------------------------------------------------
  
  residual_values <-
    model_data[[target]] -
    fitted_values
  
  
  ## ----------------------------------------------------
  ## Return
  ## ----------------------------------------------------
  
  return(
    
    list(
      
      method = "svm",
      
      model = model,
      
      formula = formula,
      
      predictors = predictors,
      
      target = target,
      
      identifier = NULL,
      
      n_train = nrow(data),
      
      n_predictors = length(predictors),
      
      fitted = fitted_values,
      
      residuals = residual_values,
      
      ## Store the actual data used for training
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