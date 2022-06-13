#' @title ObsVsPredDataMapping
#' @description  Class for mapping variables in observations vs predictions plot
#' @export
#' @family DataMapping classes
ObsVsPredDataMapping <- R6::R6Class(
  "ObsVsPredDataMapping",
  inherit = PKRatioDataMapping,
  public = list(
    #' @field smoother regression function name
    smoother = NULL,

    #' @description Create a new `ObsVsPredDataMapping` object
    #' @param lines list of lines to plot
    #' @param smoother smoother function or parameter
    #' To map a loess smoother to the plot, use `smoother`="loess"
    #' @param ... parameters inherited from `XYGDataMapping`
    #' @return A new `ObsVsPredDataMapping` object
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

#' @title ResVsPredDataMapping
#' @description  Class for mapping variables in residuals vs predictions/time plot
#' @export
#' @family DataMapping classes
ResVsPredDataMapping <- R6::R6Class(
  "ResVsPredDataMapping",
  inherit = ObsVsPredDataMapping
)

#' @title ResVsTimeDataMapping
#' @description  Class for mapping variables in residuals vs predictions/time plot
#' @export
#' @family DataMapping classes
ResVsTimeDataMapping <- R6::R6Class(
  "ResVsTimeDataMapping",
  inherit = ResVsPredDataMapping
)
