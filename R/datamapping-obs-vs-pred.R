#' @title ObsVsPredDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{obsVsPredLines} variables to \code{data}
#' @export
ObsVsPredDataMapping <- R6::R6Class(
  "ObsVsPredDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field obsVsPredLines numeric vector of limits to plot
    obsVsPredLines = NULL,
    #' @field smoother regression function name
    smoother = NULL,
    #' @field xmin numeric value xmin of \code{obsVsPredLines}
    xmin = NULL,
    #' @field xmax numeric value xmax of \code{obsVsPredLines}
    xmax = NULL,
    
    #' @description Create a new \code{ObsVsPredDataMapping} object
    #' @param obsVsPredLines numeric vector of limits to plot
    #' @param smoother smoother function or parameter
    #' To map a loess smoother to the plot, use `smoother`="loess"
    #' @param xmin numeric value xmin for of \code{obsVsPredLines}
    #' @param xmax numeric value xmax for of \code{obsVsPredLines}
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @param groupMapping R6 class \code{GroupMapping} object
    #' @param color R6 class \code{Grouping} object or its input
    #' @param fill R6 class \code{Grouping} object or its input
    #' @param linetype R6 class \code{Grouping} object or its input
    #' @param shape R6 class \code{Grouping} object or its input
    #' @param size R6 class \code{Grouping} object or its input
    #' @param data data.frame to map used by \code{smartMapping}
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{ObsVsPredDataMapping} object
    initialize = function(obsVsPredLines = 1,
                          xmin = NULL,
                          xmax = NULL,
                          smoother = NULL,
                          ...) {
      super$initialize(...)
      self$obsVsPredLines <- obsVsPredLines
      self$smoother <- smoother
      self$xmin <- xmin
      self$xmax <- xmax
    },
    
    #' @description Create a data.frame with of limits to plot
    #' This data.frame is necessary in case if log-log plots as
    #' \code{geom_abline} doesn't work properly in log scale
    #' @param data data.frame of data
    #' @return A data.frame
    getObsVsPredLines = function(data) {
      xmin <- self$xmin %||% 0.8*min(data[,self$x])
      xmax <- self$xmax %||% 1.2*max(data[,self$x])
      
      x <- 10^(seq(log10(xmin), log10(xmax), 0.01))
      y <- x * self$obsVsPredLines
      
      obsVsPredLines <- data.frame(x = x, y = y)
      return(obsVsPredLines)
    }
  )
)
