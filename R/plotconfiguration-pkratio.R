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

      # Define dummy unique value for grouping
      # Allows further modification of the aesthtic property
      if (is.null(self$legend$titles$color)) {
        mapData[, color] <- as.factor(1)
      }
      if (is.null(self$legend$titles$shape)) {
        mapData[, shape] <- as.factor(1)
      }

      plotObject <- plotObject + geom_point(
        mapping = aes(
          x = mapData$x, y = mapData$y,
          color = mapData[, color],
          shape = mapData[, shape]
        ),
        size = 1, # TO DO: discuss if this is a set entry of current Theme or part of size palette
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
