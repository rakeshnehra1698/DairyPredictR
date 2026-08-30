# ============================================================
# ARTIFICIAL NEURAL NETWORK
# ============================================================

fit_ann <- function(
    data,
    target,
    predictors = NULL,
    hidden = 5,
    linear.output = TRUE,
    threshold = 0.01,
    stepmax = 1e+05,
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
  ## Check numeric variables
  ## ----------------------------------------------------------
  
  for (v in predictors) {
    
    if (!is.numeric(model_data[[v]])) {
      
      stop(
        paste0(
          "ANN predictors must be numeric. ",
          "Variable '",
          v,
          "' is not numeric."
        )
      )
    }
  }
  
  
  if (!is.numeric(model_data[[target]])) {
    
    stop(
      paste0(
        "ANN target variable '",
        target,
        "' must be numeric."
      )
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Remove incomplete observations
  ## ----------------------------------------------------------
  
  complete_rows <- stats::complete.cases(
    model_data[, c(target, predictors), drop = FALSE]
  )
  
  if (!all(complete_rows)) {
    
    model_data <- model_data[complete_rows, , drop = FALSE]
    
  }
  
  
  ## ----------------------------------------------------------
  ## Check minimum observations
  ## ----------------------------------------------------------
  
  if (nrow(model_data) < 10) {
    
    stop(
      "Insufficient complete observations for ANN training."
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Calculate predictor scaling parameters
  ## ----------------------------------------------------------
  
  predictor_means <- sapply(
    model_data[, predictors, drop = FALSE],
    mean
  )
  
  predictor_sds <- sapply(
    model_data[, predictors, drop = FALSE],
    stats::sd
  )
  
  
  ## ----------------------------------------------------------
  ## Prevent division by zero
  ## ----------------------------------------------------------
  
  zero_sd <- predictor_sds == 0 |
    is.na(predictor_sds)
  
  if (any(zero_sd)) {
    
    stop(
      paste0(
        "Predictor(s) have zero variance: ",
        paste(
          predictors[zero_sd],
          collapse = ", "
        )
      )
    )
  }
  
  
  ## ----------------------------------------------------------
  ## Scale predictors
  ## ----------------------------------------------------------
  
  scaled_predictors <- as.data.frame(
    scale(
      model_data[, predictors, drop = FALSE],
      center = predictor_means,
      scale = predictor_sds
    )
  )
  
  
  ## ----------------------------------------------------------
  ## Scale target
  ## ----------------------------------------------------------
  
  target_mean <- mean(
    model_data[[target]]
  )
  
  target_sd <- stats::sd(
    model_data[[target]]
  )
  
  
  if (is.na(target_sd) || target_sd == 0) {
    
    stop(
      "Target variable has zero variance."
    )
  }
  
  
  scaled_target <- (
    model_data[[target]] -
      target_mean
  ) / target_sd
  
  
  ## ----------------------------------------------------------
  ## Create scaled training dataset
  ## ----------------------------------------------------------
  
  ann_data <- scaled_predictors
  
  ann_data[[target]] <- scaled_target
  
  ann_data <- ann_data[
    ,
    c(predictors, target),
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
  ## Train ANN
  ## ----------------------------------------------------------
  
  set.seed(seed)
  
  model <- neuralnet::neuralnet(
    formula = formula,
    data = ann_data,
    hidden = hidden,
    linear.output = linear.output,
    threshold = threshold,
    stepmax = stepmax,
    ...
  )
  
  
  ## ----------------------------------------------------------
  ## Training predictions on scaled data
  ## ----------------------------------------------------------
  
  training_prediction <- neuralnet::compute(
    model,
    scaled_predictors
  )
  
  
  scaled_fitted <- as.numeric(
    training_prediction$net.result
  )
  
  
  ## ----------------------------------------------------------
  ## Convert predictions back to original scale
  ## ----------------------------------------------------------
  
  fitted_values <- (
    scaled_fitted * target_sd
  ) + target_mean
  
  
  ## ----------------------------------------------------------
  ## Residuals
  ## ----------------------------------------------------------
  
  residual_values <-
    model_data[[target]] -
    fitted_values
  
  
  ## ----------------------------------------------------------
  ## Return model object
  ## ----------------------------------------------------------
  
  return(
    
    list(
      
      method = "ann",
      
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
          utils::packageVersion(
            "DairyPredictR"
          )
        ),
        error = function(e) "development"
      ),
      
      ## Scaling information
      predictor_means = predictor_means,
      
      predictor_sds = predictor_sds,
      
      target_mean = target_mean,
      
      target_sd = target_sd
      
    )
  )
}