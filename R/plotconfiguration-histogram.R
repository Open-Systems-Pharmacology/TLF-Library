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
    verticalLineProperties = NULL,

    initialize = function(title = "Histogram",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...,
                              binWidth = NULL,
                              bins = NULL,
                          verticalLineProperties = tlfEnv$currentTheme$histogramLinesProperties
                              ) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$binWidth <- binWidth
      self$bins <- bins

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
        alpha = self$theme$aesProperties$alpha[1]
      )
      
      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$fill, guides(fill = "none"))

      return(plotObject)
    },

    addVerticalLines = function(plotObject, data, metaData, dataMapping) {
      if (!is.null(dataMapping$verticalLineFunctions)) {
          
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
        
        # melt reshapes a tidy data.frame with variable names as "variable" and "value"
        tidyHelper <- reshape2::melt(aggSummary$dfHelper)
        
        # Left here, in case more refined named are asked for legend
        # verticalLineCaptions <- getDefaultCaptions(data = tidyHelper[, -ncol(tidyHelper), drop = FALSE], metaData = NULL)
        
        plotObject <- plotObject + ggplot2::geom_vline(
            data = tidyHelper,
            aes_string(
              xintercept = "value",
              color = dataMapping$groupMapping$fill$label,
              linetype = "variable"),
            size = self$verticalLineProperties$size[1],
            show.legend = TRUE
        )  
        
        # Legend linetype
        plotObject <- plotObject + guides(color = "none",
                                          linetype = guide_legend(title = "Stat", 
                                                                  labels = dataMapping$verticalLineFunctionNames,
                                                                  values = self$verticalLineProperties$linetype)) +
          scale_linetype_discrete(labels = dataMapping$verticalLineFunctionNames)
      }
      return(plotObject)
    }
  )
)
