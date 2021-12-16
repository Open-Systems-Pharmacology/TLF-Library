#' @title TornadoPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for tornado plots
#' @export
#' @family PlotConfiguration classes
TornadoPlotConfiguration <- R6::R6Class(
  "TornadoPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field bar logical setting if tornado is uses a bar plot instead of regular points
    bar = NULL,
    #' @field colorPalette color palette property from `ggplot2`
    colorPalette = NULL,
    #' @field dodge space between the bars/points
    dodge = NULL,


    #' @description Create a new `TornadoPlotConfiguration` object
    #' @param bar logical setting if tornado is uses a bar plot instead of regular points
    #' @param colorPalette color palette property from `ggplot2`
    #' @param dodge space between the bars/points
    #' @param lines `ThemeAestheticSelections` object defining properties for Tornado vertical lines
    #' @param points `ThemeAestheticSelections` object defining properties for scatter points
    #' @param ribbons `ThemeAestheticSelections` object defining properties for bars
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `TornadoPlotConfiguration` object
    initialize = function(bar = TRUE,
                          colorPalette = NULL,
                          dodge = 0.5,
                          lines = NULL,
                          points = NULL,
                          ribbons = NULL,
                          ...) {
      validateIsLogical(bar)
      validateIsString(colorPalette, nullAllowed = TRUE)
      validateIsNumeric(dodge)
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)

      super$initialize(...)

      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotTornado", propertyName = "lines")
      private$.ribbons <- ribbons %||% getThemePropertyFor(plotName = "plotTornado", propertyName = "ribbons")
      private$.points <- points %||% getThemePropertyFor(plotName = "plotTornado", propertyName = "points")
      
      self$bar <- bar
      self$colorPalette <- colorPalette
      self$dodge <- dodge
    }
  )
)
