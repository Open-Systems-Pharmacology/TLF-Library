#' @title HistogramDataMapping
#' @docType class
#' @description  Class for Histogram Group Mapping
#' @export
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XYGDataMapping,
  public = list(
    verticalLineGroupings = NULL,
    verticalLineFunctionNames = NULL,
    verticalLineFunctions = NULL,
    initialize = function(verticalLineGroupings = NULL,
                          verticalLineFunctionNames = c("mean", "median"),
                          verticalLineFunctions = c(mean,median),
                          ...) {
      super$initialize(...)
      self$verticalLineGroupings <- verticalLineGroupings
      self$verticalLineFunctionNames <- verticalLineFunctionNames
      self$verticalLineFunctions <- verticalLineFunctions
    }
  )
)
