#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class("PKRatioDataMapping",
  inherit = XYDataMapping,
  public = list(
    grouping = NULL,
    # Example of how to do some other stuff
    initialize = function(x, y, grouping) {
      self$grouping <-grouping;
      super$initialize(x, y)
    }
  )
)
