#' @title DDIRatioPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for DDI Ratio plots
#' @export
#' @family PlotConfiguration classes
DDIRatioPlotConfiguration <- R6::R6Class(
  "DDIRatioPlotConfiguration",
  inherit = PKRatioPlotConfiguration,
  public = list(
    #' @description Create a new `DDIRatioPlotConfiguration` object
    #' @param lines `ThemeAestheticSelections` object defining properties for PK ratio horizontal lines
    #' @param points `ThemeAestheticSelections` object defining properties for PK ratio scatter points
    #' @param errorbars `ThemeAestheticSelections` object defining properties for PK ratio error bars
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `DDIRatioPlotConfiguration` object
    initialize = function(lines = NULL,
                              points = NULL,
                              errorbars = NULL,
                              ...) {
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)

      super$initialize(...)

      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotDDIRatio", propertyName = "lines")
      private$.points <- points %||% getThemePropertyFor(plotName = "plotDDIRatio", propertyName = "points")
      private$.errorbars <- errorbars %||% getThemePropertyFor(plotName = "plotDDIRatio", propertyName = "errorbars")
    }
  )
)
