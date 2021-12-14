#' @title HistogramPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for histograms
#' @export
#' @family PlotConfiguration classes
HistogramPlotConfiguration <- R6::R6Class(
  "HistogramPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(

    #' @description Create a new `HistogramPlotConfiguration` object
    #' @param lines `ThemeAestheticSelections` object defining properties for vertical lines
    #' @param ribbons `ThemeAestheticSelections` object defining properties for histogram
    #' @param ylabel Histograms default display is "Count"
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `TimeProfilePlotConfiguration` object
    initialize = function(lines = NULL,
                              ribbons = NULL,
                              ylabel = "Count",
                              ...) {
      super$initialize(ylabel = ylabel, ...)

      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      private$.lines <- lines %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotHistogram$lines)
      private$.ribbons <- ribbons %||% asThemeAestheticSelections(currentTheme$plotConfigurations$plotHistogram$ribbons)
    }
  )
)
