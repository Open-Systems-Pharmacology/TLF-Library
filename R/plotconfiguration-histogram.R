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

    #verticalLineProperties = NULL,

    initialize = function(#verticalLineProperties = tlfEnv$currentTheme$verticalLineProperties,
      title = "Histogram",
      subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
      xlabel = NULL,
      ylabel = NULL,
      watermark = tlfEnv$currentTheme$watermarkText,
      data = NULL,
      metaData = NULL,
      dataMapping = NULL,
      binWidth = NULL,
      bins = NULL,
      verticalLineFunctions = NULL,
      verticalLineFunctionNames = NULL,
      ...) {

      super$initialize(...,
                       title = title,
                       subtitle = subtitle,
                       xlabel = xlabel,
                       ylabel = ylabel,
                       watermark = watermark,
                       data = data,
                       metaData = metaData,
                       dataMapping = dataMapping
      ) #self$verticalLineProperties <- verticalLineProperties

      self$binWidth = binWidth
      self$bins = bins

      self$verticalLineFunctions = verticalLineFunctions
      #print( self$verticalLineFunctions )
      self$verticalLineFunctionNames = verticalLineFunctionNames

    },


    addHistograms = function(plotObject,
                             data,
                             metaData = NULL,
                             dataMapping ,
                             binWidth = NULL , bins = NULL) {

      binWidth <- ifnotnull(binWidth,binWidth,self$binWidth)
      bins <- ifnotnull(bins,bins,self$bins)

      self$mapData <- dataMapping$getMapData(data, metaData)

      fill <- self$legend$titles$fill %||% "none"

      # Define dummy unique value for grouping
      # Allows further modification of the aesthetic property
      if (is.null(self$legend$titles$fill)) {
        self$mapData[, fill] <- as.factor(1)
      }

      plotObject <- plotObject + ggplot2::geom_histogram(
        mapping = aes(x = self$mapData$x, fill = self$mapData[ , fill ]   ),
        show.legend = TRUE,
        binwidth = binWidth,
        bins = bins,
        alpha=.5
      )

      # If no grouping is defined, remove the dummy aesthetic name from the legend
      if ("none" %in% fill) {
        plotObject <- plotObject + guides(fill = "none")
      }

      return(plotObject)
    },


    addVerticalLines = function(plotObject) {

      if (!is.null( self$verticalLineFunctions )) {

        aggSummary <-AggregationSummary$new(data = self$mapData,
                                            xColumnNames = NULL,
                                            groupingColumnNames = self$legend$titles$fill,
                                            yColumnNames = "x",
                                            aggregationFunctionsVector = self$verticalLineFunctions,
                                            aggregationFunctionNames = self$verticalLineFunctionNames,
                                            aggregationUnitsVector = NULL ,
                                            aggregationDimensionsVector = NULL )


        tidyHelper<-reshape2::melt(aggSummary$dfHelper)
        verticalLineCaptions<-getDefaultCaptions(data = tidyHelper[,-ncol(tidyHelper),drop=FALSE],metaData = NULL)


        if (TRUE){

          tidyHelper$sv <- verticalLineCaptions
          verticalLineLegendName <- "Vertical line legend"
          plotObject <- plotObject + geom_vline( data=tidyHelper,
                                                 aes(xintercept="value",  #WAS aes_string
                                                            color = "sv",
                                                            linetype = "sv" ) ) +
            scale_colour_manual(name = verticalLineLegendName,
                                values=c("red","red","blue","blue"),
                                labels=verticalLineCaptions) +
            scale_linetype_manual(name = verticalLineLegendName,
                                  values=c(1,2,1,2),
                                  labels=verticalLineCaptions)
        } else {

          plotObject <- plotObject + geom_vline( data=tidyHelper,
                                                 aes_string(xintercept="value",
                                                            color = self$legend$titles$fill,
                                                            linetype = "variable") )
        }




      }

      return(plotObject)

    }



  )
)
