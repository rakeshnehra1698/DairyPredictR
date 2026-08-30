#' Split Dairy Dataset
#'
#' Splits a dairy dataset into training and testing sets.
#'
#' @param data A data.frame.
#' @param target Character. Name of the target variable.
#' @param train_size Numeric. Proportion of data to use for training
#'   (default = 0.80).
#' @param seed Integer. Random seed for reproducibility.
#' @param verbose Logical. Print summary?
#'
#' @return A list containing:
#' \describe{
#'   \item{train}{Training dataset}
#'   \item{test}{Testing dataset}
#' }
#'
#' @examples
#' \dontrun{
#' split <- split_data(
#'   data = milk,
#'   target = "MilkYield305"
#' )
#' }
#'
#' @export

split_data <- function(data,
                       target,
                       train_size = 0.80,
                       seed = 123,
                       verbose = TRUE) {

  ## Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame.")
  }

  ## Check target
  if (!(target %in% names(data))) {
    stop(
      paste0(
        "Target variable '",
        target,
        "' not found in the dataset."
      )
    )
  }

  ## Check training proportion
  if (train_size <= 0 || train_size >= 1) {
    stop("train_size must be between 0 and 1.")
  }

  ## Set seed
  set.seed(seed)

  ## Number of observations
  n <- nrow(data)

  ## Sample indices
  train_index <- sample(
    seq_len(n),
    size = floor(train_size * n)
  )

  ## Create datasets
  train <- data[train_index, , drop = FALSE]

  test <- data[-train_index, , drop = FALSE]

  ## Summary
  if (verbose) {

    cat("\n")
    cat("=====================================\n")
    cat(" DairyPredictR Data Split Summary\n")
    cat("=====================================\n")
    cat("Total observations :", n, "\n")
    cat("Training set       :", nrow(train), "\n")
    cat("Testing set        :", nrow(test), "\n")
    cat("Training proportion:", train_size, "\n")
    cat("Random seed        :", seed, "\n")
    cat("=====================================\n\n")

  }

  ## Return list
  return(
    list(
      train = train,
      test = test,
      train_index = train_index,
      test_index = setdiff(seq_len(n), train_index),
      target = target
    )
  )
}
