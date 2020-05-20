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
    
    #' @description Create a new \code{TimeProfilePlotConfiguration} object
    #' @param bins numeric vector of bin edges
    #' @param binWidth numeric value of bin width
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(binWidth = NULL,
                              bins = NULL,
                              ...) {
      super$initialize(...)

      self$binWidth <- binWidth
      self$bins <- bins %||% tlfEnv$defaultAggregation$bins
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
      binWidth <- binWidth %||% self$binWidth
      bins <- bins %||% self$bins

      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + ggplot2::geom_histogram(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, fill = mapLabels$fill, color = mapLabels$fill),
        show.legend = TRUE,
        binwidth = binWidth,
        bins = bins,
        alpha = self$theme$aesProperties$alpha[1]
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("legendLabels", mapLabels$fill, guides(fill = "none", color="none"))

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
