#' @title TimeProfilePlotConfiguration
#' @docType class
#' @description  Plot Configuration for Time Profile Plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    LLOQLinesProperties = NULL,
    
    initialize = function(LLOQLinesProperties = tlfEnv$currentTheme$LLOQLinesProperties,
                              title = "Time Profile Plot",
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

      self$LLOQLinesProperties <- LLOQLinesProperties
      
    },

    addLLOQLines = function(LLOQLines, plotObject) {
      if (!is.null(LLOQLines)) {
        for (LLOQIndex in seq(1, length(LLOQLines))) {
          plotObject <- plotObject +
            ggplot2::geom_hline(
              yintercept = LLOQLines[LLOQIndex],
              linetype = self$LLOQLinesProperties$linetype[LLOQIndex],
              color = self$LLOQLinesProperties$color[LLOQIndex],
              size = self$LLOQLinesProperties$size[LLOQIndex]
            )
        }
      }
      return(plotObject)
    },

    addTimeProfiles = function(plotObject, data, metaData, dataMapping) {
      mapData <- dataMapping$getMapData(data, metaData)

      color <- self$legend$titles$color %||% "none"
      shape <- self$legend$titles$shape %||% "none"
      linetype <- self$legend$titles$linetype %||% "none"
      fill <- self$legend$titles$fill %||% "none"
      
      # Define dummy unique value for grouping
      # Allows further modification of the aesthtic property
      if (is.null(self$legend$titles$color)){mapData[, color] <- as.factor(1)}
      if (is.null(self$legend$titles$shape)){mapData[, shape] <- as.factor(1)}
      if (is.null(self$legend$titles$linetype)){mapData[, linetype] <- as.factor(1)}
      if (is.null(self$legend$titles$fill)){mapData[, fill] <- as.factor(1)}
      
      if (!is.null(dataMapping$y)) {
        if (!is.null(dataMapping$yMin) && !is.null(dataMapping$yMax)) {
          plotObject <- plotObject + ggplot2::geom_errorbar(
            mapping = aes(x = mapData$x, ymin = mapData$yMin, ymax = mapData$yMax, 
                          color = mapData[,color], linetype = mapData[,linetype]),
            size = 1,
            show.legend = TRUE
          )
        }
        plotObject <- plotObject + ggplot2::geom_line(
          mapping = aes(x = mapData$x, y = mapData$y, 
                        color = mapData[,color], linetype = mapData[,linetype]),
          size = 1,
          show.legend = TRUE
        )
      } else {
        plotObject <- plotObject + ggplot2::geom_ribbon(
          mapping = aes(x = mapData$x, ymin = mapData$yMin, ymax = mapData$yMax, 
                        fill = mapData[,fill]),
          show.legend = TRUE
        )
      }
      
      # If no grouping is defined, remove the dummy aesthtic name from the legend
      if ("none" %in% linetype) {plotObject <- plotObject + guides(linetype = "none")}
      if ("none" %in% color) {plotObject <- plotObject + guides(color = "none")}
      if ("none" %in% fill) {plotObject <- plotObject + guides(fill = "none")}
      if ("none" %in% shape) {plotObject <- plotObject + guides(shape = "none")}
      
      return(plotObject)
    }
  )
)
