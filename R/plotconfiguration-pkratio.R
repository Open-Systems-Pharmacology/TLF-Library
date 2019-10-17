#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    pkRatioLinesProperties = NULL,
    colorGrouping = NULL,
    shapeGrouping = NULL,

    initialize = function(pkRatioLinesProperties = tlfEnv$currentTheme$pkRatioLinesProperties,
                              colorGrouping = NULL,
                              shapeGrouping = NULL,
                              title = "PK Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = tlfEnv$currentTheme$watermarkText,
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

      if (!is.null(dataMapping$groupings)) {
        self$colorGrouping <- dataMapping$groupings[["color"]]$group
        self$shapeGrouping <- dataMapping$groupings[["shape"]]$group
      }

      # Overwrite grouping if config is different from Mapping
      self$colorGrouping <- colorGrouping %||% self$colorGrouping
      self$shapeGrouping <- shapeGrouping %||% self$shapeGrouping
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

      color <- self$legend$titles$color %||% "none"
      shape <- self$legend$titles$shape %||% "none"

      plotObject <- plotObject + geom_point(
        mapping = aes(
          x = mapData$x, y = mapData$y,
          color = mapData[, color],
          shape = mapData[, shape]
        ),
        size = 1, # To be updated with current Theme
        show.legend = TRUE
      )

      # If no grouping is defined, remove the dummy aesthtic name from the legend
      if ("none" %in% shape) {
        plotObject <- plotObject + guides(shape = "none")
      }
      if ("none" %in% color) {
        plotObject <- plotObject + guides(color = "none")
      }

      return(plotObject)
    }
  )
)
