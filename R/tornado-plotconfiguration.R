#' @title TornadoPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for tornado plots
#' @export
TornadoPlotConfiguration <- R6::R6Class(
  "TornadoPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field tornadoCaption list of properties for tornado plot specific features
    tornadoCaption = NULL,
    #' @field bar logical setting if tornado is uses a bar plot instead of regular points
    bar = NULL,
    #' @field colorPalette color palette property from `ggplot2`
    colorPalette = NULL,
    #' @field dodge space between the bars/points
    dodge = NULL,


    #' @description Create a new \code{TornadoPlotConfiguration} object
    #' @param tornadoCaption list of properties for tornado plot specific features
    #' @param bar logical setting if tornado is uses a bar plot instead of regular points
    #' @param colorPalette color palette property from `ggplot2`
    #' @param dodge space between the bars/points
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TornadoPlotConfiguration} object
    initialize = function(tornadoCaption = getDefaultCaptionFor("tornado"),
                              bar = TRUE,
                              colorPalette = NULL,
                              dodge = 0.5,
                              ...) {
      validateIsOfType(tornadoCaption, "data.frame")
      #validateIsIncluded(names(tornadoCaption), CaptionProperties)
      # Currently the properties from tornadoCaption are not defined in the theme snapshot
      validateIsLogical(bar)
      validateIsString(colorPalette, nullAllowed = TRUE)
      validateIsNumeric(dodge)

      super$initialize(...)
      self$bar <- bar
      self$colorPalette <- colorPalette
      self$dodge <- dodge
      self$tornadoCaption <- tornadoCaption
    }
  )
)
