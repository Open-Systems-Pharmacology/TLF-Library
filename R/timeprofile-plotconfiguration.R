#' @title TimeProfilePlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for time profile plots
#' @export
#' @family PlotConfiguration classes
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @description Create a new `TimeProfilePlotConfiguration` object
    #' @param lines `ThemeAestheticSelections` defining properties of lines
    #' @param ribbons `ThemeAestheticSelections` defining properties of ribbons
    #' @param points `ThemeAestheticSelections` defining properties of points
    #' @param errorbars `ThemeAestheticSelections` defining properties of error bars
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `TimeProfilePlotConfiguration` object
    initialize = function(lines = NULL,
                          ribbons = NULL,
                          points = NULL,
                          errorbars = NULL,
                          ...) {
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)

      super$initialize(...)
      
      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotTimeProfile", propertyName = "lines")
      private$.ribbons <- ribbons %||% getThemePropertyFor(plotName = "plotTimeProfile", propertyName = "ribbons")
      private$.points <- points %||% getThemePropertyFor(plotName = "plotTimeProfile", propertyName = "points")
      private$.errorbars <- errorbars %||% getThemePropertyFor(plotName = "plotTimeProfile", propertyName = "errorbars")
    }
  )
)
