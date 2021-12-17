#' @title ObsVsPredPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Obs vs Pred plots
#' @export
#' @family PlotConfiguration classes
ObsVsPredPlotConfiguration <- R6::R6Class(
  "ObsVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @description Create a new `ObsVsPredPlotConfiguration` object
    #' @param lines `ThemeAestheticSelections` object defining properties for lines
    #' @param points `ThemeAestheticSelections` object defining properties for scatter points
    #' @param errorbars `ThemeAestheticSelections` object defining properties for error bars
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `ObsVsPredPlotConfiguration` object
    initialize = function(lines = NULL,
                          points = NULL,
                          errorbars = NULL,
                          ...) {
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(errorbars, "ThemeAestheticSelections", nullAllowed = TRUE)

      super$initialize(...)

      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotObsVsPred", propertyName = "lines")
      private$.points <- points %||% getThemePropertyFor(plotName = "plotObsVsPred", propertyName = "points")
      private$.errorbars <- errorbars %||% getThemePropertyFor(plotName = "plotObsVsPred", propertyName = "errorbars")
    }
  )
)

#' @title ResVsPredPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Res vs Pred/Time plots
#' @export
ResVsPredPlotConfiguration <- R6::R6Class(
  "ResVsPredPlotConfiguration",
  inherit = ObsVsPredPlotConfiguration
)
