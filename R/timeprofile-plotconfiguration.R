#' @title TimeProfilePlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for time profile plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @field timeProfileCaption list of properties for time profile plot specific features
    timeProfileCaption = NULL,

    #' @description Create a new \code{TimeProfilePlotConfiguration} object
    #' @param timeProfileCaption list of properties for PK ratio plot specific features
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(timeProfileCaption = getDefaultCaptionFor("timeProfile"),
                              ...) {
      validateIsOfType(timeProfileCaption, "data.frame")
      validateIsIncluded(names(timeProfileCaption), CaptionProperties)
      super$initialize(...)

      self$timeProfileCaption <- timeProfileCaption
    }
  )
)
