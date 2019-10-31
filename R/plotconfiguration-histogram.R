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
        alpha=.5,
        bins = bins
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


        #aggSummary$dfHelper$mean <- 10*aggSummary$dfHelper$mean
        #print(aggSummary$dfHelper)

        rm<-reshape2::melt(aggSummary$dfHelper)
        print(rm)

        for (n in seq(1,length(aggSummary$aggregationFunctionsVector))) {


          plotObject <- plotObject + geom_vline( data=rm,
                                                 aes_string(xintercept="value",
                                                            color = self$legend$titles$fill,
                                                            linetype = "variable") )

          #print(aggSummary$dfHelper)

          # plotObject <- plotObject + geom_vline( data=aggSummary$dfHelper ,
          #                aes_string(xintercept=aggSummary$aggregationFunctionNames[n],
          #                           color = self$legend$titles$fill) )

          # plotObject <- plotObject + geom_vline(data=sr, aes(xintercept=mean, colour=Grp))


          # plotObject <- plotObject +  geom_vline( mapping =  aes_string(xintercept = aggSummary$dfHelper[[ aggSummary$aggregationFunctionNames[n] ]] ) ,
          #                                         color = cls[n],
          #                                         linetype = ltp[n])


          }

      }

      return(plotObject)

    }



  )
)
