#' @title HistogramDataMapping
#' @description  R6 class for mapping \code{x}, \code{verticalLineGroupings}, 
#' \code{verticalLineFunctionNames} and \code{verticalLineFunctions} variables to \code{data}
#' @export
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field verticalLineGroupings R6 class \code{Grouping} variable
    verticalLineGroupings = NULL,
    #' @field verticalLineFunctionNames Vector of function name 
    #' to be indicated in captions of the histogram
    verticalLineFunctionNames = NULL,
    #' @field verticalLineFunctions List of functions calculated on \code{data} 
    #' to obtain vertical lines on the histogram
    verticalLineFunctions = NULL,
    
    #' @description Create a new \code{HistogramDataMapping} object
    #' @param x Name of variable to map
    #' @param y Name of variable to map
    #' @param verticalLineGroupings R6 class \code{Grouping} variable
    #' @param verticalLineFunctionNames Vector of function name to be indicated in captions of the histogram
    #' Default value uses `mean` and `median`.
    #' @param verticalLineFunctions List of functions calculated on \code{data} 
    #' Default value uses `mean` and `median`.
    #' @param data data.frame to map used by \code{smartMapping} 
    #' @return A new \code{HistogramDataMapping} object
    initialize = function(verticalLineGroupings = NULL,
                              verticalLineFunctionNames = c("mean", "median"),
                              verticalLineFunctions = c(mean, median),
                              ...) {
      super$initialize(...)
      self$verticalLineGroupings <- verticalLineGroupings
      self$verticalLineFunctionNames <- verticalLineFunctionNames
      self$verticalLineFunctions <- verticalLineFunctions
    }
  )
)
