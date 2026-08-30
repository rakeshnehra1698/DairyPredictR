#' Fit a Machine Learning Model
#'
#' Trains a machine learning model for milk yield prediction.
#'
#' @param data A data.frame containing the training data.
#' @param target Character. Name of the target variable.
#' @param method Character. Machine learning algorithm.
#' @param ... Additional arguments passed to the selected model fitting function.
#' Supported methods are:
#' "linear", "rf", "gbm", "svm", "ann".
#' @param predictors Character vector. Names of predictor variables.
#' @param identifier Character. Optional name of the identifier variable.
#' @return A DairyPredictModel object.
#'
#' @examples
#' \dontrun{
#' model <- fit_model(
#'   data = split$train,
#'   target = "MilkYield305",
#'   method = "linear"
#' )
#' }
#'
#' @export

# ============================================================
# MAIN MODEL FITTING FUNCTION
# ============================================================

fit_model <- function(
    data,
    target,
    predictors,
    identifier = NULL,
    method = "linear",
    ...
) {
  
  ## ----------------------------------------------------------
  ## Prepare and validate variable roles
  ## ----------------------------------------------------------
  
  prepared <- prepare_model_data(
    data = data,
    target = target,
    predictors = predictors,
    identifier = identifier
  )
  
  
  ## ----------------------------------------------------------
  ## Extract validated information
  ## ----------------------------------------------------------
  
  model_data <- prepared$data
  
  target <- prepared$target
  
  predictors <- prepared$predictors
  
  identifier <- prepared$identifier
  
  
  ## ----------------------------------------------------------
  ## Select algorithm
  ## ----------------------------------------------------------
  
  trainer <- model_dispatcher(method)
  
  
  ## ----------------------------------------------------------
  ## Train model
  ## ONLY target + predictors are passed to trainer
  ## ----------------------------------------------------------
  
  fitted <- trainer(
    data = model_data,
    target = target,
    predictors = predictors,
    ...
  )
  
  
  ## ----------------------------------------------------------
  ## Store variable roles
  ## ----------------------------------------------------------
  
  fitted$target <- target
  
  fitted$predictors <- predictors
  
  fitted$identifier <- identifier
  
  
  ## ----------------------------------------------------------
  ## Assign package class
  ## ----------------------------------------------------------
  
  class(fitted) <- unique(c(
    "DairyPredictModel",
    class(fitted)
  ))
  
  
  ## ----------------------------------------------------------
  ## Return fitted model
  ## ----------------------------------------------------------
  
  return(fitted)
  
}
