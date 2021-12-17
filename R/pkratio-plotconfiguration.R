#' @title PKRatioPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for PK ratio plots
#' @export
#' @family PlotConfiguration classes
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(

    #' @description Create a new `PKRatioPlotConfiguration` object
    #' @param lines `ThemeAestheticSelections` object defining properties for PK ratio horizontal lines
    #' @param points `ThemeAestheticSelections` object defining properties for PK ratio scatter points
    #' @param errorbars `ThemeAestheticSelections` object defining properties for PK ratio error bars
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `PKRatioPlotConfiguration` object
    initialize = function(lines = NULL,
                          points = NULL,
                          errorbars = NULL,
                          ...) {
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)
      
      super$initialize(...)

      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotPKRatio", propertyName = "lines")
      private$.points <- points %||% getThemePropertyFor(plotName = "plotPKRatio", propertyName = "points")
      private$.errorbars <- errorbars %||% getThemePropertyFor(plotName = "plotPKRatio", propertyName = "errorbars")
    }
  )
)
