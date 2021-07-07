#' @title ObsVsPredDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{obsVsPredLines} variables to \code{data}
#' @export
ObsVsPredDataMapping <- R6::R6Class(
  "ObsVsPredDataMapping",
  inherit = PKRatioDataMapping,
  public = list(
    #' @field smoother regression function name
    smoother = NULL,

    #' @description Create a new \code{ObsVsPredDataMapping} object
    #' @param lines list of lines to plot
    #' @param smoother smoother function or parameter
    #' To map a loess smoother to the plot, use `smoother`="loess"
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{ObsVsPredDataMapping} object
    initialize = function(lines = DefaultDataMappingValues$obsVsPred,
                              smoother = NULL,
                              ...) {
      validateIsIncluded(smoother, c("lm", "loess"), nullAllowed = TRUE)

      super$initialize(...)
      self$lines <- lines
      self$smoother <- smoother
    }
  )
)
