#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYDataMapping,
  public = list(
    # Example of how to do some other stuff
    initialize = function(x = "Age",
                              y = "Ratio", ...) {
      super$initialize(x, y, ...)
    }
  )
)
