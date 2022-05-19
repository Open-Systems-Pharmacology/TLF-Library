#' @title HistogramDataMapping
#' @description  R6 class for mapping `x`, `bins`, `binwidth`,`stack` and `distribution` to `data`
#' @export
#' @family DataMapping classes
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field stack logical defining if histogram bars should be stacked
    stack = NULL,
    #' @field bins number of bins or binning values/methods passed on `ggplot2::geom_histogram`
    bins = NULL,
    #' @field binwidth width of bins passed on `ggplot2::geom_histogram`. Overwrites `bins`
    binwidth = NULL,
    #' @field distribution Name of distribution to fit to the data.
    #' Only 2 distributions are currently available: `"normal"` and `"logNormal"`
    distribution = NULL,

    #' @description Create a new `HistogramDataMapping` object
    #' @param stack logical defining if histogram bars should be stacked
    #' @param bins argument passed on `ggplot2::geom_histogram`
    #' @param binwidth width of bins passed on `ggplot2::geom_histogram`. Overwrites `bins`
    #' @param distribution Name of distribution to fit to the data.
    #' Only 2 distributions are currently available: `"normal"` and `"logNormal"`
    #' @param ... parameters inherited from `XYGDataMapping`
    #' @return A new `HistogramDataMapping` object
    initialize = function(stack = FALSE,
                              bins = NULL,
                              binwidth = NULL,
                              distribution = NULL,
                              ...) {
      super$initialize(...)
      validateIsLogical(stack)
      validateIsIncluded(distribution, c("none", "normal", "logNormal"), nullAllowed = TRUE)

      self$stack <- stack
      self$bins <- bins %||% tlfEnv$defaultAggregation$bins
      self$binwidth <- binwidth
      self$distribution <- distribution %||% "none"
    }
  )
)
