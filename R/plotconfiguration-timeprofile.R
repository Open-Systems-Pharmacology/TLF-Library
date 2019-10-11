#' @title TimeProfilePlotConfiguration
#' @docType class
#' @description  Plot Configuration for Time Profile Plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    LLOQLinesProperties = NULL,
    colorGrouping = NULL,
    linetypeGrouping = NULL,

    initialize = function(LLOQLinesProperties = tlfEnv$currentTheme$LLOQLinesProperties,
                              colorGrouping = NULL,
                              linetypeGrouping = NULL,
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

      if (!is.null(dataMapping$groupings)) {
        self$colorGrouping <- dataMapping$groupings[["color"]]$groupingName[[1]]
        self$linetypeGrouping <- dataMapping$groupings[["linetype"]]$groupingName[[1]]
      }

      # Overwrite grouping if config is different from Mapping
      self$colorGrouping <- colorGrouping %||% self$colorGrouping
      self$linetypeGrouping <- linetypeGrouping %||% self$linetypeGrouping
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

      # If no grouping is defined, the aesthetic variables still need to be included
      # with a dummy name to be removed, otherwise it is impossible to modify them later on.
      if (is.null(self$linetypeGrouping)) {
        linetype <- "linetype"
      }
      if (is.null(self$colorGrouping)) {
        color <- "color"
      }

      if (!is.null(dataMapping$y)) {
        if (!is.null(dataMapping$yMin) && !is.null(dataMapping$yMax)) {
          plotObject <- plotObject + ggplot2::geom_errorbar(
            data = mapData,
            mapping = aes(x = x, ymin = yMin, ymax = yMax, color = color, linetype = linetype),
            size = 1,
            show.legend = TRUE
          )
        }
        plotObject <- plotObject + ggplot2::geom_line(
          data = mapData,
          mapping = aes(x = x, y = y, color = color, linetype = linetype),
          size = 1,
          show.legend = TRUE
        )
      } else {
        plotObject <- plotObject + ggplot2::geom_ribbon(
          data = mapData,
          mapping = aes(x = x, ymin = yMin, ymax = yMax, fill = color),
          show.legend = TRUE
        )
      }

      # If no grouping is defined, remove the dummy aesthtic name from the legend
      if (is.null(self$linetypeGrouping)) {
        plotObject <- plotObject + guides(linetype = "none")
      }
      if (is.null(self$colorGrouping)) {
        plotObject <- plotObject + guides(color = "none")
      }

      return(plotObject)
    }
  )
)
