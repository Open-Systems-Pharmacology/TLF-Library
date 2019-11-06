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

      # In case there is no mapping, set dummy aesthetic label corresponding to constant factor
      # Allows further changes in the configuration later on using scales
      mapData$defaultAes <- factor("")
      colorLabel <- dataMapping$groupings$color$label %||% "defaultAes"
      shapeLabel <- dataMapping$groupings$shape$label %||% "defaultAes"
      sizeLabel <- dataMapping$groupings$size$label %||% "defaultAes"
      fillLabel <- dataMapping$groupings$fill$label %||% "defaultAes"
      linetypeLabel <- dataMapping$groupings$linetype$label %||% "defaultAes"

      if (!is.null(dataMapping$y)) {
        if (!is.null(dataMapping$yMin) && !is.null(dataMapping$yMax)) {
          plotObject <- plotObject + ggplot2::geom_errorbar(
            mapping = aes(
              x = mapData$x, ymin = mapData$yMin, ymax = mapData$yMax,
              color = mapData[, colorLabel], linetype = mapData[, linetypeLabel], size = mapData[, sizeLabel]
            ),
            show.legend = TRUE
          )
        }
        plotObject <- plotObject + ggplot2::geom_line(
          mapping = aes(
            x = mapData$x, y = mapData$y,
            color = mapData[, colorLabel], linetype = mapData[, linetypeLabel], size = mapData[, sizeLabel]
          ),
          show.legend = TRUE
        )
        # If no mapping defined, remove dummy aesthetic label from the legend
        plotObject <- plotObject +
          ifequal("defaultAes", colorLabel, guides(color = "none")) +
          ifequal("defaultAes", linetypeLabel, guides(linetype = "none")) +
          ifequal("defaultAes", sizeLabel, guides(size = "none"))
      } else {
        plotObject <- plotObject + ggplot2::geom_ribbon(
          mapping = aes(
            x = mapData$x, ymin = mapData$yMin, ymax = mapData$yMax,
            fill = mapData[, fillLabel]
          ),
          show.legend = TRUE
        )

        # If no mapping defined, remove dummy aesthetic label from the legend
        plotObject <- plotObject +
          ifequal("defaultAes", fillLabel, guides(fill = "none"))
      }

      return(plotObject)
    }
  )
)
