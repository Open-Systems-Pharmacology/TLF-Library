#' @title HistogramDataMapping
#' @description  R6 class for mapping `x`, `verticalLineGroupings`,
#' `verticalLineFunctionNames` and `verticalLineFunctions` variables to `data`
#' @export
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field stack logical defining if histogram bars should be stacked
    stack = NULL,
    #' @field bins number of bins or binning values/methods passed on `ggplot2::geom_histogram`
    bins = NULL,
    #' @field lines values or functions to define vertical lines
    lines = NULL,
    #' @field fitNormalDist logical defining if a normal distribution should be fitted
    fitNormalDist = NULL,
    #' @field fitDensity logical defining if a density distribution function should be fitted
    fitDensity = NULL,

    #' @description Create a new `HistogramDataMapping` object
    #' @param stack logical defining if histogram bars should be stacked
    #' @param bins argument passed on `ggplot2::geom_histogram`
    #' @param lines values or functions to define vertical lines
    #' @param fitNormalDist logical defining if a normal distribution should be fitted
    #' @param fitDensity logical defining if a density distribution should be fitted
    #' @param ... parameters inherited from `XYGDataMapping`
    #' @return A new `HistogramDataMapping` object
    initialize = function(stack = FALSE,
                          bins = NULL,
                          lines = DefaultDataMappingValues$histogram,
                          fitNormalDist = FALSE,
                          fitDensity = FALSE,
                          ...) {
      super$initialize(...)
      validateIsLogical(stack)
      validateIsLogical(fitNormalDist)
      validateIsLogical(fitDensity)
      self$stack <- stack
      self$fitNormalDist <- fitNormalDist
      self$fitDensity <- fitDensity

      self$bins <- bins
      self$lines <- lines
    }
  )
)
