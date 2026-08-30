#' Clean Dairy Data
#'
#' Performs basic cleaning of a dairy dataset.
#'
#' @param data A data.frame.
#' @param remove_duplicates Logical. Remove duplicate rows?
#' @param trim_whitespace Logical. Remove leading/trailing spaces?
#' @param convert_factors Logical. Convert character columns to factors?
#' @param verbose Logical. Print a cleaning summary?
#'
#' @return A cleaned data.frame.
#'
#' @export

clean_data <- function(data,
                       remove_duplicates = TRUE,
                       trim_whitespace = TRUE,
                       convert_factors = FALSE,
                       verbose = TRUE) {

  ## Check input
  if (!is.data.frame(data)) {
    stop("Input must be a data.frame.")
  }

  original_rows <- nrow(data)

  ## Remove duplicate rows
  duplicates_removed <- 0

  if (remove_duplicates) {
    duplicates_removed <- sum(duplicated(data))
    data <- unique(data)
  }

  ## Trim whitespace
  if (trim_whitespace) {

    char_cols <- sapply(data, is.character)

    data[char_cols] <- lapply(
      data[char_cols],
      trimws
    )

  }

  ## Convert character columns to factors
  if (convert_factors) {

    char_cols <- sapply(data, is.character)

    data[char_cols] <- lapply(
      data[char_cols],
      as.factor
    )

  }

  ## Missing values
  missing_summary <- colSums(is.na(data))

  ## Print summary
  if (verbose) {

    cat("\n")
    cat("=====================================\n")
    cat(" DairyPredictR Data Cleaning Summary\n")
    cat("=====================================\n")
    cat("Rows                :", original_rows, "\n")
    cat("Rows after cleaning :", nrow(data), "\n")
    cat("Duplicates removed  :", duplicates_removed, "\n")
    cat("Missing values      :", sum(missing_summary), "\n")
    cat("=====================================\n\n")

    if (sum(missing_summary) > 0) {
      print(missing_summary[missing_summary > 0])
    }

  }

  return(data)}
