#' @title PKRatioPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for PK ratio plots
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(

    #' @description Create a new \code{PKRatioPlotConfiguration} object
    #' @param lines `ThemeAestheticSelections` object defining properties for PK ratio horizontal lines
    #' @param points `ThemeAestheticSelections` object defining properties for PK ratio scatter points
    #' @param errorbars `ThemeAestheticSelections` object defining properties for PK ratio error bars
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{PKRatioPlotConfiguration} object
    initialize = function(lines = NULL,
                              points = NULL,
                              errorbars = NULL,
                              ...) {
      super$initialize(...)

      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.lines <- lines %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotPKRatio$lines)
      private$.points <- points %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotPKRatio$points)
      private$.errorbars <- errorbars %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotPKRatio$errorbars)
    }
  )
)
