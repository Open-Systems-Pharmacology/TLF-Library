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
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)

      super$initialize(ylabel = ylabel, ...)

      private$.ribbons <- ribbons %||% getThemePropertyFor(plotName = "plotHistogram", propertyName = "ribbons")
      private$.lines <- lines %||% getThemePropertyFor(plotName = "plotHistogram", propertyName = "lines")
    }
  )
)
