# ======================================================
# DairyPredictR
#
# Internal model dispatcher
#
# Not exported
# ======================================================
#' @keywords internal
#' @noRd
model_dispatcher <- function(method) {

  method <- tolower(method)

  supported_methods <- c(
    "linear",
    "rf",
    "gbm",
    "svm",
    "ann"
  )


  if (!(method %in% supported_methods)) {

    stop(
      paste0(
        "Unsupported method: '",
        method,
        "'.\n\nSupported methods are:\n",
        paste(supported_methods, collapse = ", ")
      )
    )

  }

  switch(
    method,
    linear = fit_linear,
    rf = fit_rf,
    gbm = fit_gbm,
    svm = fit_svm,
    ann = fit_ann
  )

}
