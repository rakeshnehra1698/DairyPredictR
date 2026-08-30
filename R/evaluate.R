#' Evaluate Model Performance
#'
#' Evaluate predictions from a fitted DairyPredictModel.
#'
#' @param object A DairyPredictModel object.
#' @param newdata A data.frame containing the test dataset.
#' @param actual Character. Name of the observed target variable.
#' @param digits Number of decimal places.
#' @param verbose Logical. Print results?
#' @param ... Additional arguments.
#'
#' @return A data.frame containing evaluation metrics.
#' @importFrom stats cor predict
#' @export

evaluate <- function(object,
                     newdata,
                     actual = object$target,
                     digits = 4,
                     verbose = TRUE,
                     ...) {

  ## Checks
  if (!inherits(object, "DairyPredictModel")) {
    stop("object must be a DairyPredictModel.")
  }

  if (!is.data.frame(newdata)) {
    stop("newdata must be a data.frame.")
  }

  if (!(actual %in% names(newdata))) {
    stop("Actual response variable not found.")
  }

  ## Predictions
  predicted <- predict(
    object,
    newdata = newdata
  )

  observed <- newdata[[actual]]

  ## Metrics
  rmse <- sqrt(mean((observed - predicted)^2))

  mae <- mean(abs(observed - predicted))

  mape <- mean(abs((observed - predicted) / observed)) * 100

  bias <- mean(predicted - observed)

  r2 <- cor(observed, predicted)^2

  results <- data.frame(

    Metric = c(
      "R2",
      "RMSE",
      "MAE",
      "MAPE",
      "Bias"
    ),

    Value = round(
      c(
        r2,
        rmse,
        mae,
        mape,
        bias
      ),
      digits
    )

  )

  if (verbose) {

    cat("\n")
    cat("=====================================\n")
    cat(" DairyPredictR Model Evaluation\n")
    cat("=====================================\n")

    print(results)

    cat("=====================================\n\n")

  }

  return(results)

}
