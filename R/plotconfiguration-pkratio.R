#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Class for PKRatio Plot Configuration
#' @field legend R6 class defining legendConfiguration
#' @field xAxis R6 class defining xAxisConfiguration
#' @field yAxis R6 class defining yAxisConfiguration
#' @field background R6 class defining backgroundConfiguration
#' @field theme R6 class defining theme aesthtic properties
#' @field filename Name of the saved plot
#' @field pkRatioProperties Properties of PK ratio plot specific features
#' @section Methods:
#' \describe{
#' \item{new(...)}{Initialize PKRatioPlotConfiguration}
#' \item{setPlotProperties(plotObject)}{Apply properties of plot labels.}
#' \item{setPlotBackground(plotObject)}{Apply background properties to plot.}
#' \item{savePlot(plotObject)}{Save ggplot as file.}
#' \item{addPKRatioLines(plotObject, pkRatioLines)}{Add PK ratio horizontal lines to plot.}
#' \item{addPKRatios(plotObject, data, metaData, dataMapping)}{Add PK ratios to plot.}
#' }
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    pkRatioProperties = NULL,

    initialize = function(pkRatioProperties = tlfEnv$currentTheme$pkRatio,
                              title = "PK Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$pkRatioProperties <- pkRatioProperties
    },

    addPKRatioLines = function(plotObject, pkRatioLines) {
      for (RatioIndex in seq(1, length(pkRatioLines))) {
        plotObject <- plotObject +
          ggplot2::geom_hline(
            yintercept = pkRatioLines[RatioIndex],
            linetype = self$pkRatioProperties$lines$linetype[RatioIndex],
            color = self$pkRatioProperties$lines$color[RatioIndex],
            size = self$pkRatioProperties$lines$size[RatioIndex]
          )
      }
      return(plotObject)
    },

    addPKRatios = function(plotObject, data, metaData, dataMapping) {
      # Check if mapping is included in the data
      # Add the group mapping and aesthtics variables in the data.frame
      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + geom_point(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          shape = mapLabels$shape,
          size = mapLabels$size
        ),
        show.legend = TRUE
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$shape, guides(shape = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
