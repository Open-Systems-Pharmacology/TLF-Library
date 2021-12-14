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
      super$initialize(...)

      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)
      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      private$.lines <- lines %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotDDIRatio$lines)
      private$.points <- points %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotDDIRatio$points)
      private$.errorbars <- errorbars %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotDDIRatio$errorbars)
    }
  )
)
