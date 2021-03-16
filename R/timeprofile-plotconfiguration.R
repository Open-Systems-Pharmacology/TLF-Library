#' @title TimeProfilePlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for time profile plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @description Create a new \code{TimeProfilePlotConfiguration} object
    #' @param lines `ThemeAestheticSelections` defining properties of lines
    #' @param ribbons `ThemeAestheticSelections` defining properties of ribbons
    #' @param points `ThemeAestheticSelections` defining properties of points
    #' @param errorbars `ThemeAestheticSelections` defining properties of error bars
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(lines = NULL,
                              ribbons = NULL,
                              points = NULL,
                              errorbars = NULL,
                              ...) {
      super$initialize(...)
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)
      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      private$.lines <- lines %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotTimeProfile$lines)
      private$.ribbons <- ribbons %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotTimeProfile$ribbons)
      private$.points <- points %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotTimeProfile$points)
      private$.errorbars <- errorbars %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotTimeProfile$errorbars)
    }
  )
)
