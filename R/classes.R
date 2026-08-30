#' Print a DairyPredictModel
#'
#' Prints a summary of a DairyPredictR model.
#'
#' @param x A DairyPredictModel object.
#' @param ... Additional arguments.
#'
#' @return Invisibly returns the model.
#'
#' @export

print.DairyPredictModel <- function(x, ...) {

  cat("\n")
  cat("========================================\n")
  cat("         DairyPredictR Model\n")
  cat("========================================\n")

  cat("Algorithm        :", x$method, "\n")

  cat("Target Variable  :", x$target, "\n")

  cat("Predictors       :", length(x$predictors), "\n")

  cat("Predictor Names  :\n")

  cat("  ",
      paste(x$predictors, collapse = ", "),
      "\n")

  if (!is.null(x$training_rows)) {

    cat("Training Rows    :", x$training_rows, "\n")

  }

  if (!is.null(x$training_columns)) {

    cat("Training Columns :", x$training_columns, "\n")

  }

  if (!is.null(x$trained_on)) {

    cat("Trained On       :", as.character(x$trained_on), "\n")

  }

  cat("========================================\n")

  invisible(x)

}
