#' @title HistogramPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for histograms
#' @export
HistogramPlotConfiguration <- R6::R6Class(
  "HistogramPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @field mapData data.frame after dataMapping
    mapData = NULL,
    #' @field bins numeric vector of bin edges
    bins = NULL,
    #' @field binWidth numeric value of bin width
    binWidth = NULL,
    #' @field histogramProperties list of properties for histogram specific features
    histogramProperties = NULL,

    #' @description Create a new \code{TimeProfilePlotConfiguration} object
    #' @param bins numeric vector of bin edges
    #' @param binWidth numeric value of bin width
    #' @param histogramProperties list of properties for PK ratio plot specific features
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param legend R6 class \code{LegendConfiguration} object defining legend properties
    #' @param legendTitles List of legend titles
    #' @param xAxis R6 class \code{XAxisConfiguration} object defining X-axis properties
    #' @param xScale character defining X-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param xLimits numeric vector of X-axis limits
    #' @param yAxis R6 class \code{YAxisConfiguration} object defining X-axis properties
    #' @param yScale character defining Y-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param yLimits numeric vector of Y-axis limits
    #' @param background R6 class \code{BackgroundConfiguration} defining background properties
    #' @param watermark R6 class \code{Label} object defining watermark background
    #' @param saveConfiguration R6 class \code{SaveConfiguration} defining saving properties
    #' @param filename character defining the name of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param data data.frame used by \code{smartMapping}
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class or subclass \code{XYGDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(title = "Histogram",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              binWidth = NULL,
                              bins = NULL,
                              histogramProperties = tlfEnv$currentTheme$histogram,
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$binWidth <- binWidth
      self$bins <- bins

      self$histogramProperties <- histogramProperties
    },

    #' @description Add histogram as histogram layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{HistogramDataMapping}
    #' @param bins numeric vector of bin edges
    #' @param binWidth numeric value of bin width
    #' @return A \code{ggplot} object with histogram
    addHistograms = function(plotObject,
                                 data,
                                 metaData = NULL,
                                 dataMapping,
                                 binWidth = NULL, bins = NULL) {
      binWidth <- ifnotnull(binWidth, binWidth, self$binWidth)
      bins <- ifnotnull(bins, bins, self$bins)

      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + ggplot2::geom_histogram(
        data = mapData,
        mapping = aes_string(x = mapLabels$x, fill = mapLabels$fill),
        show.legend = TRUE,
        binwidth = binWidth,
        bins = bins,
        alpha = self$theme$aesProperties$alpha[1]
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$fill, guides(fill = "none"))

      return(plotObject)
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
