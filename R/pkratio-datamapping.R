#' @title PKRatioDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{pkRatioLines} variables to \code{data}
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field pkRatioValues numeric vector of ratio limits to plot
    pkRatioValues = NULL,

    #' @description Create a new \code{PKRatioDataMapping} object
    #' @param pkRatioValues numeric vector of ratio limits to plot
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{PKRatioDataMapping} object
    initialize = function(pkRatioValues = DefaultDataMappingValues$pkRatio,
                              ...) {
      super$initialize(...)
      self$pkRatioValues <- pkRatioValues
    }
  )
)
