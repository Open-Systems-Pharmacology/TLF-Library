#' @title ObsVsPredPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for Obs vs Pred plots
#' @export
ObsVsPredPlotConfiguration <- R6::R6Class(
  "ObsVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @description Create a new \code{ObsVsPredPlotConfiguration} object
    #' @param lines `ThemeAestheticSelections` object defining properties for lines
    #' @param points `ThemeAestheticSelections` object defining properties for scatter points
    #' @param errorbars `ThemeAestheticSelections` object defining properties for error bars
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{ObsVsPredPlotConfiguration} object
    initialize = function(lines = NULL,
                              points = NULL,
                              errorbars = NULL,
                              ...) {
      super$initialize(...)

      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)

      private$.lines <- lines %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotObsVsPred$lines)
      private$.points <- points %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotObsVsPred$points)
      private$.errorbars <- errorbars %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotObsVsPred$errorbars)
    }
  )
)
