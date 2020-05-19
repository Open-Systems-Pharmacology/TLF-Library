#' @title PKRatioPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for PK ratio plots
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field pkRatioCaption list of properties for PK ratio plot specific features
    pkRatioCaption = NULL,

    #' @description Create a new \code{PKRatioPlotConfiguration} object
    #' @param pkRatioCaption list of properties for PK ratio plot specific features
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{PKRatioPlotConfiguration} object
    initialize = function(pkRatioCaption = getDefaultCaptionFor("pkRatio"),
                              ...) {
      validateIsOfType(pkRatioCaption, "data.frame")
      validateIsIncluded(names(pkRatioCaption), CaptionProperties)
      super$initialize(...)

      self$pkRatioCaption <- pkRatioCaption
    }
  )
)
