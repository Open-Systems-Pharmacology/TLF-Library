#' @title BoxWhiskerPlotConfiguration
#' @description  R6 class defining the configuration of a `ggplot` object
#' @export
#' @family PlotConfiguration classes
BoxWhiskerPlotConfiguration <- R6::R6Class(
  "BoxWhiskerPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @description Create a new `PKRatioPlotConfiguration` object
    #' @param ribbons `ThemeAestheticSelections` object defining properties for boxes of boxplot
    #' @param points `ThemeAestheticSelections` object defining properties for outlier scatter points
    #' @param outliers logical defining if outliers should be included in boxplot
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `PKRatioPlotConfiguration` object
    initialize = function(outliers = TRUE,
                          ribbons = NULL,
                          points = NULL,
                          ...) {
      super$initialize(...)

      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(points, "ThemeAestheticSelections", nullAllowed = TRUE)
      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      private$.ribbons <- ribbons %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotBoxWhisker$ribbons)
      private$.points <- points %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotBoxWhisker$points)

      validateIsLogical(outliers)
      private$.outliers <- outliers
    }
  ),
  active = list(
    #' @field outliers logical defining if outliers should be included in boxplot
    outliers = function(value) {
      if (missing(value)) {
        return(private$.outliers)
      }
      validateIsLogical(value, nullAllowed = TRUE)
      private$.outliers <- value %||% private$.outliers
      return(invisible())
    }
  ),
  private = list(
    .outliers = NULL
  )
)
