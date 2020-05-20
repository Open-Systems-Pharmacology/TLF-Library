#' @title ObsVsPredDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{obsVsPredLines} variables to \code{data}
#' @export
ObsVsPredDataMapping <- R6::R6Class(
  "ObsVsPredDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field obsVsPredValues numeric vector of limits to plot
    obsVsPredValues = NULL,
    #' @field lloq numeric value of lower limit of quantification
    lloq = NULL,
    #' @field smoother regression function name
    smoother = NULL,
    #' @field range 2 elements vector of x limits
    range = NULL,

    #' @description Create a new \code{ObsVsPredDataMapping} object
    #' @param obsVsPredValues list of values for obs vs pred plot
    #' @param lloq numeric value of lower limit of quantification
    #' @param smoother smoother function or parameter
    #' To map a loess smoother to the plot, use `smoother`="loess"
    #' @param range 2 elements vector of x limits
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{ObsVsPredDataMapping} object
    initialize = function(obsVsPredValues = DefaultDataMappingValues$obsVsPred,
                              lloq = NULL,
                              range = NULL,
                              smoother = NULL,
                              ...) {
      super$initialize(...)
      self$obsVsPredValues <- obsVsPredValues
      self$lloq <- lloq
      self$smoother <- smoother
      self$range <- range
    },

    #' @description Create a data.frame with of limits to plot
    #' This data.frame is necessary in case if log-log plots as
    #' \code{geom_abline} doesn't work properly in log scale
    #' @param data data.frame of data
    #' @return A data.frame
    getObsVsPredLines = function(data) {
      xmin <- ifnotnull(self$range, min(self$range), 0.8 * min(data[, self$x]))
      xmax <- ifnotnull(self$range, max(self$range), 1.2 * max(data[, self$x]))

      x <- seq(xmin, xmax, 0.01)
      if (xmin > 0) {
        10^(seq(log10(xmin), log10(xmax), 0.01))
      }
      y <- x * self$obsVsPredValues$`y=x`

      obsVsPredLines <- data.frame(x = x, y = y)
      return(obsVsPredLines)
    }
  )
)
