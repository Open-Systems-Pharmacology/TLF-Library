#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    pkRatioLines = NULL,
    # Example of how to do some other stuff
    initialize = function(pkRatioLines = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
                          x = "Age",
                          y = "Ratio", ...) {
      super$initialize(x=x, y=y, ...)
      self$pkRatioLines <- pkRatioLines
    }
  )
)
