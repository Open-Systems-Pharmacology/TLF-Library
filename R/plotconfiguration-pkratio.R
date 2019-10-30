#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    pkRatioLinesProperties = NULL,

    initialize = function(pkRatioLinesProperties = tlfEnv$currentTheme$pkRatioLinesProperties,
                              title = "PK Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = tlfEnv$currentTheme$watermark,
                              data = NULL,
                              metaData = NULL,
                              dataMapping = NULL,
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        xlabel = xlabel,
        ylabel = ylabel,
        watermark = watermark,
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      )

      self$pkRatioLinesProperties <- pkRatioLinesProperties
    },

    addPKRatioLines = function(pkRatioLines, plotObject) {
      for (RatioIndex in seq(1, length(pkRatioLines))) {
        plotObject <- plotObject +
          ggplot2::geom_hline(
            yintercept = pkRatioLines[RatioIndex],
            linetype = self$pkRatioLinesProperties$linetype[RatioIndex],
            color = self$pkRatioLinesProperties$color[RatioIndex],
            size = self$pkRatioLinesProperties$size[RatioIndex]
          )
      }
      return(plotObject)
    },

    addPKRatios = function(plotObject, data, metaData, dataMapping) {
      
      mapData <- dataMapping$getMapData(data, metaData)
      
      # In case there is no mapping, set dummy aesthetic label corresponding to constant factor
      # Allows further changes in the configuration later on using scales
      mapData$defaultAes <- factor("")
      colorLabel <- dataMapping$groupings$color$label %||% "defaultAes"
      shapeLabel <- dataMapping$groupings$shape$label %||% "defaultAes"
      sizeLabel <- dataMapping$groupings$size$label %||% "defaultAes"
      
      plotObject <- plotObject + geom_point(
        mapping = aes(
          x = mapData$x, 
          y = mapData$y,
          color = mapData[,colorLabel],
          shape = mapData[,shapeLabel],
          size = mapData[,sizeLabel]
        ),
        show.legend = TRUE
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject + 
          ifequal("defaultAes", colorLabel, guides(color = "none")) + 
          ifequal("defaultAes", shapeLabel, guides(shape = "none")) + 
          ifequal("defaultAes", sizeLabel, guides(size = "none")) 
      
      return(plotObject)
    }
  )
)
