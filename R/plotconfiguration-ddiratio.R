#' @title DDIRatioPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for DDI Ratio plots
#' @export
DDIRatioPlotConfiguration <- R6::R6Class(
  "DDIRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field ddiRatioProperties list of properties for DDI ratio plot specific features
    ddiRatioProperties = NULL,

    #' @description Create a new \code{BoxWhiskerPlotConfiguration} object
    #' @param ddiRatioProperties list of properties for DDI ratio plot specific features
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
    #' @param dataMapping R6 class or subclass \code{XYDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{BoxWhiskerPlotConfiguration} object
    initialize = function(ddiRatioProperties = tlfEnv$currentTheme$ddiRatio,
                              title = "DDI Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$ddiRatioProperties <- ddiRatioProperties
    },

    #' @description Add DDI ratio limits as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param dataMapping R6 class \code{DDIRatioDataMapping}
    #' @return A \code{ggplot} object with DDI ratio limits
    addDDIRatioLines = function(plotObject, dataMapping) {
      # geom_abline don't work properly when plot scale is log
      ddiRatioLines <- dataMapping$getDDIRatioLines()
      # Remove x from names
      ddiRatioLabels <- utils::tail(names(ddiRatioLines), -1)
      for (RatioIndex in seq(1, length(ddiRatioLabels))) {
        plotObject <- plotObject +
          ggplot2::geom_line(
            data = ddiRatioLines,
            mapping = aes_string(x = "x", y = ddiRatioLabels[RatioIndex]),
            linetype = self$ddiRatioProperties$lines$linetype[RatioIndex],
            color = self$ddiRatioProperties$lines$color[RatioIndex],
            size = self$ddiRatioProperties$lines$size[RatioIndex]
          )
      }
      return(plotObject)
    },

    #' @description Add Guest et al ratio limits as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param dataMapping R6 class \code{DDIRatioDataMapping}
    #' @return A \code{ggplot} object with Guest et al ratio limits
    addGuestLines = function(plotObject, dataMapping) {
      guestLines <- dataMapping$getGuestLines()
      plotObject <- plotObject +
        ggplot2::geom_line(
          data = guestLines,
          mapping = aes_string(x = "x", y = "ymax"),
          linetype = self$ddiRatioProperties$guest$linetype,
          color = self$ddiRatioProperties$guest$color,
          size = self$ddiRatioProperties$guest$size
        ) +
        ggplot2::geom_line(
          data = guestLines,
          mapping = aes_string(x = "x", y = "ymin"),
          linetype = self$ddiRatioProperties$guest$linetype,
          color = self$ddiRatioProperties$guest$color,
          size = self$ddiRatioProperties$guest$size
        )
      return(plotObject)
    },

    #' @description Add DDI ratios as scatter layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{DDIRatioDataMapping}
    #' @return A \code{ggplot} object DDI ratios
    addDDIRatios = function(plotObject, data, metaData, dataMapping) {
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
