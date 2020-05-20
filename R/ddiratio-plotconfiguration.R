#' @title DDIRatioPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for DDI Ratio plots
#' @export
DDIRatioPlotConfiguration <- R6::R6Class(
  "DDIRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field ddiRatioCaption list of properties for DDI ratio plot specific features
    ddiRatioCaption = NULL,

    #' @description Create a new \code{DDIRatioPlotConfiguration} object
    #' @param ddiRatioCaption list of properties for DDI ratio plot specific features
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{DDIRatioPlotConfiguration} object
    initialize = function(ddiRatioCaption = getDefaultCaptionFor("ddiRatio"),
                              ...) {
      validateIsOfType(ddiRatioCaption, "data.frame")
      validateIsIncluded(names(ddiRatioCaption), CaptionProperties)
      super$initialize(...)

      self$ddiRatioCaption <- ddiRatioCaption
    }
  )
)
