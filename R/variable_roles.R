# ============================================================
# VARIABLE ROLE VALIDATION
# ============================================================

validate_variables <- function(
    data,
    target,
    predictors,
    identifier = NULL
) {
  
  # ----------------------------------------------------------
  # Check data
  # ----------------------------------------------------------
  
  if (!is.data.frame(data)) {
    stop("Input data must be a data.frame.")
  }
  
  
  # ----------------------------------------------------------
  # Check target
  # ----------------------------------------------------------
  
  if (length(target) != 1 || is.na(target)) {
    stop("Exactly one target variable must be specified.")
  }
  
  if (!(target %in% names(data))) {
    stop(
      paste0(
        "Target variable '",
        target,
        "' was not found in the dataset."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Check identifier
  # ----------------------------------------------------------
  
  if (!is.null(identifier)) {
    
    if (length(identifier) != 1 || is.na(identifier)) {
      stop(
        "Identifier must contain exactly one variable."
      )
    }
    
    if (!(identifier %in% names(data))) {
      stop(
        paste0(
          "Identifier variable '",
          identifier,
          "' was not found in the dataset."
        )
      )
    }
    
  }
  
  
  # ----------------------------------------------------------
  # Check predictors
  # ----------------------------------------------------------
  
  if (is.null(predictors) ||
      length(predictors) == 0) {
    
    stop(
      "At least one predictor variable must be specified."
    )
  }
  
  
  # Remove duplicated predictors
  
  predictors <- unique(predictors)
  
  
  # Check predictor names
  
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
  
  
  # ----------------------------------------------------------
  # Prevent TARGET from being a predictor
  # ----------------------------------------------------------
  
  if (target %in% predictors) {
    
    stop(
      paste0(
        "Target variable '",
        target,
        "' cannot also be used as a predictor."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Prevent IDENTIFIER from being a predictor
  # ----------------------------------------------------------
  
  if (!is.null(identifier) &&
      identifier %in% predictors) {
    
    stop(
      paste0(
        "Identifier variable '",
        identifier,
        "' cannot be used as a predictor."
      )
    )
  }
  
  
  # ----------------------------------------------------------
  # Check overlap between target and identifier
  # ----------------------------------------------------------
  
  if (!is.null(identifier) &&
      target == identifier) {
    
    stop(
      "Target and identifier must be different variables."
    )
  }
  
  
  # ----------------------------------------------------------
  # Return variable roles
  # ----------------------------------------------------------
  
  list(
    
    target = target,
    
    identifier = identifier,
    
    predictors = predictors
    
  )
}

# ============================================================
# CREATE MODELING DATA
# ============================================================

prepare_model_data <- function(
    data,
    target,
    predictors,
    identifier = NULL
) {
  
  roles <- validate_variables(
    data = data,
    target = target,
    predictors = predictors,
    identifier = identifier
  )
  
  
  # ----------------------------------------------------------
  # Only target + predictors enter the model
  # ----------------------------------------------------------
  
  model_data <- data[
    ,
    c(
      roles$target,
      roles$predictors
    ),
    drop = FALSE
  ]
  
  
  # ----------------------------------------------------------
  # Return complete variable-role information
  # ----------------------------------------------------------
  
  list(
    
    data = model_data,
    
    target = roles$target,
    
    identifier = roles$identifier,
    
    predictors = roles$predictors
    
  )
}