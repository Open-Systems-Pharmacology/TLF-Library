#' @title HistogramPlotConfiguration
#' @docType class
#' @description  Plot Configuration for Histograms
#' @export
HistogramPlotConfiguration <- R6::R6Class(
  "HistogramPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    mapData = NULL,
    bins = NULL,
    binWidth = NULL,
    histogramProperties = NULL,

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
        numHistograms <- length(levels(  summaryDataFrame[[dataMapping$groupMapping$fill$label]]  ))


        # Left here, in case more refined named are asked for legend
        summaryDataFrame$summaryCaptions <- getDefaultCaptions(data = summaryDataFrame[, -ncol(summaryDataFrame), drop = FALSE], metaData = NULL)

        legendLabels <- levels(summaryDataFrame$summaryCaptions)

        plotObject <- plotObject + ggplot2::geom_vline(
          data = summaryDataFrame,
          aes_string( xintercept = "value", color = "summaryCaptions", linetype = "summaryCaptions") , size = 1) + scale_colour_manual(
            name = "Summary",
            values = fillVec[ rep(seq(1,numHistograms) , each= numVerticalLineFunctions) ] ,
            labels = legendLabels
          )  + scale_linetype_manual(
            name = "Summary",
            values = rep(seq(1,numVerticalLineFunctions),numHistograms),
            labels = legendLabels
          )


      }
      return(plotObject)
    }
  )
)
