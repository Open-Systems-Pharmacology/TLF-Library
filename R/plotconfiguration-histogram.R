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
    verticalLineFunctions = NULL,
    verticalLineFunctionNames = NULL,
    verticalLineProperties = NULL,

    initialize = function(verticalLineProperties = tlfEnv$currentTheme$histogramLinesProperties,
                              title = "Histogram",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...,
                              binWidth = NULL,
                              bins = NULL,
                              verticalLineFunctions = NULL,
                              verticalLineFunctionNames = NULL
                              ) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$binWidth <- binWidth
      self$bins <- bins

      self$verticalLineFunctions <- verticalLineFunctions
      self$verticalLineFunctionNames <- verticalLineFunctionNames
      self$verticalLineProperties <- verticalLineProperties
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
        alpha = theme$aesProperties$alpha
      )
      
      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$fill, guides(fill = "none"))

      return(plotObject)
    },

    addVerticalLines = function(data, metaData, dataMapping, plotObject) {
      if (!is.null(self$verticalLineFunctions)) {
        aggSummary <- AggregationSummary$new(
          data = self$mapData,
          xColumnNames = NULL,
          groupingColumnNames = self$legend$titles$fill,
          yColumnNames = "x",
          aggregationFunctionsVector = self$verticalLineFunctions,
          aggregationFunctionNames = self$verticalLineFunctionNames,
          aggregationUnitsVector = NULL,
          aggregationDimensionsVector = NULL
        )

        tidyHelper <- reshape2::melt(aggSummary$dfHelper)
        verticalLineCaptions <- getDefaultCaptions(data = tidyHelper[, -ncol(tidyHelper), drop = FALSE], metaData = NULL)


        if (TRUE) {
          tidyHelper$sv <- verticalLineCaptions
          verticalLineLegendName <- "Vertical line legend"
          plotObject <- plotObject + geom_vline(
            data = tidyHelper,
            aes(
              xintercept = "value", # WAS aes_string
              color = "sv",
              linetype = "sv"
            )
          ) +
            scale_colour_manual(
              name = verticalLineLegendName,
              values = c("red", "red", "blue", "blue"),
              labels = verticalLineCaptions
            ) +
            scale_linetype_manual(
              name = verticalLineLegendName,
              values = c(1, 2, 1, 2),
              labels = verticalLineCaptions
            )
        } else {
          plotObject <- plotObject + geom_vline(
            data = tidyHelper,
            aes_string(
              xintercept = "value",
              color = self$legend$titles$fill,
              linetype = "variable"
            )
          )
        }
      }

      return(plotObject)
    }
  )
)
