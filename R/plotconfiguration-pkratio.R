#' @title PKRatioPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for PK ratio plots
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field pkRatioProperties list of properties for PK ratio plot specific features
    pkRatioProperties = NULL,

    #' @description Create a new \code{PKRatioPlotConfiguration} object
    #' @param pkRatioProperties list of properties for PK ratio plot specific features
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param legend R6 class \code{LegendConfiguration} object defining legend properties
    #' @param legendTitles List of legend titles
    #' @param xAxis R6 class \code{XAxisConfiguration} object defining X-axis properties
    #' @param xScale character defining X-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param xLimits numeric vector of X-axis limits
    #' @param yAxis R6 class \code{YAxisConfiguration} object defining X-axis properties
    #' @param yScale character defining Y-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param yLimits numeric vector of Y-axis limits
    #' @param background R6 class \code{BackgroundConfiguration} defining background properties
    #' @param watermark R6 class \code{Label} object defining watermark background
    #' @param saveConfiguration R6 class \code{SaveConfiguration} defining saving properties
    #' @param filename character defining the name of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param data data.frame used by \code{smartMapping}
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class or subclass \code{XYGDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{PKRatioPlotConfiguration} object
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

    #' @description Add PK ratio limits as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param dataMapping R6 class \code{PKRatioDataMapping}
    #' @return A \code{ggplot} object with PK ratio limits
    addPKRatioLines = function(plotObject, dataMapping) {
      pkRatioLines <- dataMapping$pkRatioLines
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

    #' @description Add PK ratios as scatter layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{PKRatioDataMapping}
    #' @return A \code{ggplot} object with PK ratios
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
