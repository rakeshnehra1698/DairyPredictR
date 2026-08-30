#' Read Dairy Data
#'
#' Read dairy production data from CSV or Excel files.
#'
#' @param file Character. Path to the input file (.csv or .xlsx).
#'
#' @return A data.frame containing the imported data.
#'
#' @examples
#' \dontrun{
#' milk <- read_dairy("milk_data.csv")
#' milk <- read_dairy("milk_data.xlsx")
#' }
#'
#' @export

read_dairy <- function(file) {

  if (!file.exists(file)) {
    stop("File not found: ", file)
  }

  ext <- tolower(tools::file_ext(file))

  if (ext == "csv") {

    data <- utils::read.csv(
      file,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

  } else if (ext %in% c("xlsx", "xls")) {

    data <- readxl::read_excel(file)

    data <- as.data.frame(data)

  } else {

    stop(
      "Unsupported file format.\n",
      "Supported formats are: .csv, .xlsx, .xls"
    )

  }

  return(data)
}
#' Save a DairyPredictR Model
#'
#' Save a fitted DairyPredictModel to an RDS file.
#'
#' @param object A fitted DairyPredictModel object.
#' @param file Character. Path where the model will be saved.
#'
#' @return Invisibly returns the file path.
#'
#' @export

save_model <- function(object, file) {
  
  ## Check model
  if (!inherits(object, "DairyPredictModel")) {
    
    stop(
      "object must be a DairyPredictModel."
    )
    
  }
  
  ## Check file
  if (!is.character(file) || length(file) != 1) {
    
    stop(
      "file must be a single character path."
    )
    
  }
  
  ## Save model
  saveRDS(
    object,
    file = file
  )
  
  invisible(file)
}
#' Load a DairyPredictR Model
#'
#' Load a previously saved DairyPredictModel from an RDS file.
#'
#' @param file Character. Path to the saved model.
#'
#' @return A DairyPredictModel object.
#'
#' @export

load_model <- function(file) {
  
  ## Check file
  if (!file.exists(file)) {
    
    stop(
      "Model file not found: ",
      file
    )
    
  }
  
  ## Load model
  object <- readRDS(file)
  
  ## Check loaded object
  if (!inherits(object, "DairyPredictModel")) {
    
    stop(
      "The selected file is not a valid DairyPredictModel."
    )
    
  }
  
  return(object)
}