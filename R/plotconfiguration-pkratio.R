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
                              watermark = NULL,
                              data = NULL,
                              metaData = NULL,
                              dataMapping = NULL,
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        xlabel = xlabel,
        ylabel = ylabel,
        watermark = watermark %||% tlfEnv$currentTheme$watermark,
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
      # mapData <- dataMapping$getMapData(data, metaData)
      mapData <- dataMapping$checkMapData(data, metaData)

      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + geom_point(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          shape = mapLabels$shape
          # size = mapLabels$size TO DO add size into Themes
        ),
        show.legend = TRUE
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifequal("defaultAes", mapLabels$color, guides(color = "none")) +
        ifequal("defaultAes", mapLabels$shape, guides(shape = "none")) #+
      # ifequal("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
