#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    pkRatioLinesProperties = NULL,

    initialize = function(title = "PK Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              pkRatioLinesProperties = tlfEnv$currentTheme$pkRatioLinesProperties,
                              ...) {
      super$initialize(
        title = title,
        ...
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
    
    addPKRatios = function(mapData, plotObject){
      plotObject <- plotObject + geom_point(mapping=aes(
        x = x, y = y,
        color = color,
        shape = shape,
        size = size
      ),
      data = mapData,
      show.legend = TRUE
      )
      return(plotObject)
      
    }
  )
)
