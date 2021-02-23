#' @title HistogramPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for histograms
#' @export
HistogramPlotConfiguration <- R6::R6Class(
  "HistogramPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(

    #' @description Create a new \code{HistogramPlotConfiguration} object
    #' @param lines `ThemeAestheticSelections` object defining properties for vertical lines
    #' @param ribbons `ThemeAestheticSelections` object defining properties for histogram
    #' @param ylabel Histograms default display is "Count"
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(lines = NULL,
                              ribbons = NULL,
                              ylabel = "Count",
                              ...) {
      super$initialize(ylabel = ylabel, ...)

      validateIsOfType(lines, "ThemeAestheticSelections", nullAllowed = TRUE)
      validateIsOfType(ribbons, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.lines <- lines %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotHistogram$lines)
      private$.ribbons <- ribbons %||% asThemeAestheticSelections(tlfEnv$currentTheme$plotConfigurations$plotHistogram$ribbons)
    },

    #' @description Add statistics as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{HistogramDataMapping}
    #' @return A \code{ggplot} object
    addVerticalLines = function(plotObject, data, metaData, dataMapping) {
      if (!is.null(dataMapping$verticalLineFunctions)) {
        fillVec <- tlfEnv$currentTheme$aesProperties$fill

        mapData <- dataMapping$checkMapData(data, metaData)

        aggSummary <- AggregationSummary$new(
          data = mapData,
          xColumnNames = NULL,
          groupingColumnNames = dataMapping$groupMapping$fill$label,
          yColumnNames = dataMapping$x,
          aggregationFunctionsVector = dataMapping$verticalLineFunctions,
          aggregationFunctionNames = dataMapping$verticalLineFunctionNames,
          aggregationUnitsVector = NULL,
          aggregationDimensionsVector = NULL
        )

        numVerticalLineFunctions <- length(dataMapping$verticalLineFunctions)

        # melt reshapes a tidy data.frame with variable names as "variable" and "value"
        summaryDataFrame <- reshape2::melt(aggSummary$dfHelper)
        numHistograms <- length(levels(summaryDataFrame[[dataMapping$groupMapping$fill$label]]))


        # Left here, in case more refined named are asked for legend
        summaryDataFrame$summaryCaptions <- getDefaultCaptions(data = summaryDataFrame[, -ncol(summaryDataFrame), drop = FALSE], metaData = NULL)

        legendLabels <- levels(summaryDataFrame$summaryCaptions)

        plotObject <- plotObject + ggplot2::geom_vline(
          data = summaryDataFrame,
          aes_string(xintercept = "value", color = "summaryCaptions", linetype = "summaryCaptions"), size = 1
        ) + scale_colour_manual(
          name = "Summary",
          values = fillVec[ rep(seq(1, numHistograms), each = numVerticalLineFunctions) ],
          labels = legendLabels
        ) + scale_linetype_manual(
          name = "Summary",
          values = rep(seq(1, numVerticalLineFunctions), numHistograms),
          labels = legendLabels
        )
      }
      return(plotObject)
    }
  )
)
