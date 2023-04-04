#' @title PKRatioPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for PK ratio plots
#' @field defaultYScale Default yAxis scale value when creating a `PKRatioPlotConfiguration` object
#' @export
#' @family PlotConfiguration classes
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    defaultYScale = "log"
  )
)

#' @title DDIRatioPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for DDI ratio plots
#' @field defaultXScale Default xAxis scale value when creating a `DDIRatioPlotConfiguration` object
#' @field defaultYScale Default yAxis scale value when creating a `DDIRatioPlotConfiguration` object
#' @field defaultExpand Default expand value when creating a `DDIRatioPlotConfiguration` object
#' @export
#' @family PlotConfiguration classes
DDIRatioPlotConfiguration <- R6::R6Class(
  "DDIRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    defaultXScale = "log",
    defaultYScale = "log",
    defaultExpand = TRUE
  )
)

#' @title ObsVsPredPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Obs vs Pred plots
#' @field defaultSymmetricAxes Default option setting symmetric xAxis and/or yAxis limits when creating a `ObsVsPredPlotConfiguration` object
#' @field lloqDirection Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y (both).
#' @field foldLinesLegend Whether to draw fold lines in legend. default to FALSE.
#' @field foldLinesLegendDiagonal Whether to draw diagonal lines in legend for fold lines. default to FALSE.
#' @field foldLineslegendType translation of `foldLinesLegendDiagonal` in geom type.
#' @export
#' @family PlotConfiguration classes
ObsVsPredPlotConfiguration <- R6::R6Class(
  "ObsVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    defaultSymmetricAxes = TRUE,
    lloqDirection = NULL,
    foldLinesLegend = NULL,
    foldLinesLegendDiagonal = NULL,
    foldLineslegendType = NULL,
    #' @description Create a new `ObsVsPredPlotConfiguration` object
    #' @param lloqDirection Whether to draw LLOQ lines for x (vertical), y (horizontal) or x and y (both).
    #' @param foldLinesLegend Whether to draw fold lines in legend. default to FALSE.
    #' @param foldLinesLegendDiagonal Whether to draw diagonal lines in legend for fold lines. default to FALSE.
    #' @param legendPosition Where to draw the legend. default to "insideTopLeft". Available values in `LegendPositions`
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `CumulativeTimeProfilePlotConfiguration` object
    initialize = function(lloqDirection = "vertical",
                          foldLinesLegend = FALSE,
                          foldLinesLegendDiagonal = FALSE,
                          legendPosition = "insideTopLeft",
                          ...) {
      super$initialize(legendPosition = legendPosition, ...)
      validateEnumValue(lloqDirection, Directions)
      self$lloqDirection <- lloqDirection
      self$foldLinesLegend <- foldLinesLegend
      self$foldLinesLegendDiagonal <- foldLinesLegendDiagonal
      self$foldLineslegendType <- if (self$foldLinesLegendDiagonal) {
        "abline"
      } else {
        "path"
      }
    }
  )
)

#' @title ResVsPredPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Res vs Pred/Time plots
#' @field defaultSymmetricAxes Default option setting symmetric xAxis and/or yAxis limits when creating a `ResVsPredPlotConfiguration` object
#' @export
#' @family PlotConfiguration classes
ResVsPredPlotConfiguration <- R6::R6Class(
  "ResVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    defaultSymmetricAxes = TRUE
  )
)

#' @title ResVsTimePlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Res vs Pred/Time plots
#' @export
#' @family PlotConfiguration classes
ResVsTimePlotConfiguration <- R6::R6Class(
  "ResVsTimePlotConfiguration",
  inherit = ResVsPredPlotConfiguration
)

#' @title HistogramPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for histograms
#' @export
#' @family PlotConfiguration classes
HistogramPlotConfiguration <- R6::R6Class(
  "HistogramPlotConfiguration",
  inherit = PlotConfiguration
)

#' @title QQPlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for Quantile-Quantile plots
#' @export
#' @family PlotConfiguration classes
QQPlotConfiguration <- R6::R6Class(
  "QQPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @description Create a new `QQPlotConfiguration` object
    #' @param xlabel QQ-plot default display is "Standard Normal Quantiles"
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `QQPlotConfiguration` object
    initialize = function(xlabel = "Standard Normal Quantiles",
                          ...) {
      super$initialize(xlabel = xlabel, ...)
    }
  )
)

#' @title CumulativeTimeProfilePlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object for cumulative time profile plots
#' @field defaultExpand Default expand value when creating a `CumulativeTimeProfilePlotConfiguration` object
#' @field colorPalette color palette property from `ggplot2`
#' @export
#' @family PlotConfiguration classes
CumulativeTimeProfilePlotConfiguration <- R6::R6Class(
  "CumulativeTimeProfilePlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    defaultExpand = TRUE,
    colorPalette = NULL,

    #' @description Create a new `CumulativeTimeProfilePlotConfiguration` object
    #' @param colorPalette color palette property from `ggplot2`
    #' @param ... parameters inherited from `PlotConfiguration`
    #' @return A new `CumulativeTimeProfilePlotConfiguration` object
    initialize = function(colorPalette = NULL, ...) {
      validateIsString(colorPalette, nullAllowed = TRUE)
      super$initialize(...)
      self$colorPalette <- colorPalette
    }
  )
)
