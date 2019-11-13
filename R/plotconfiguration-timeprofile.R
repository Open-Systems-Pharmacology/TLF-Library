#' @title TimeProfilePlotConfiguration
#' @docType class
#' @description  Plot Configuration for Time Profile Plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    lloqLinesProperties = NULL,

    initialize = function(lloqLinesProperties = tlfEnv$currentTheme$lloqLinesProperties,
                              title = "Time Profile Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$lloqLinesProperties <- lloqLinesProperties
    },

    addLLOQLines = function(metaData, dataMapping, plotObject) {
      if (is.null(metaData)) {
        return(plotObject)
      }

      LLOQLines <- ifnotnull(
        dataMapping$y,
        metaData[[dataMapping$y]]$LLOQ,
        metaData[[dataMapping$yMin]]$LLOQ
      )

      if (!is.null(LLOQLines)) {
        for (LLOQIndex in seq(1, length(LLOQLines))) {
          plotObject <- plotObject +
            ggplot2::geom_hline(
              yintercept = LLOQLines[LLOQIndex],
              linetype = self$lloqLinesProperties$linetype[LLOQIndex],
              color = self$lloqLinesProperties$color[LLOQIndex],
              size = self$lloqLinesProperties$size[LLOQIndex]
            )
        }
      }
      return(plotObject)
    },

    addTimeProfiles = function(plotObject, data, metaData, dataMapping) {
      # Check if mapping is included in the data
      # Add the group mapping and aesthtics variables in the data.frame
      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      if (!is.null(dataMapping$y)) {
        # Plot error bars if yMin and yMax defined
        if (!is.null(dataMapping$yMin) && !is.null(dataMapping$yMax)) {
          # Shape is not an input of geom_errorbar
          plotObject <- plotObject + ggplot2::geom_errorbar(
            data = mapData,
            mapping = aes_string(
              x = mapLabels$x, ymin = mapLabels$yMin, ymax = mapLabels$yMax,
              color = mapLabels$color, size = mapLabels$size
            ),
            show.legend = TRUE
          )
        }
        # Plot time profile on top
        # Priority of time profiles Shape > Linetype
        if (!is.null(dataMapping$groupMapping$linetype$group)) {
          plotObject <- plotObject + ggplot2::geom_line(
            data = mapData,
            mapping = aes_string(
              x = mapLabels$x, y = mapLabels$y,
              color = mapLabels$color, linetype = mapLabels$linetype, size = mapLabels$size
            ),
            show.legend = TRUE
          )

          plotObject <- plotObject +
            ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
            ifEqual("defaultAes", mapLabels$linetype, guides(linetype = "none")) +
            ifEqual("defaultAes", mapLabels$size, guides(size = "none"))
        } else {
          plotObject <- plotObject + ggplot2::geom_point(
            data = mapData,
            mapping = aes_string(
              x = mapLabels$x, y = mapLabels$y,
              color = mapLabels$color, shape = mapLabels$shape, size = mapLabels$size
            ),
            show.legend = TRUE
          )

          plotObject <- plotObject +
            ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
            ifEqual("defaultAes", mapLabels$shape, guides(shape = "none")) +
            ifEqual("defaultAes", mapLabels$size, guides(size = "none"))
        }
      } else {
        # Plot shaded area
        plotObject <- plotObject + ggplot2::geom_ribbon(
          data = mapData,
          mapping = aes_string(
            x = mapLabels$x, ymin = mapLabels$yMin, ymax = mapLabels$yMax,
            fill = mapLabels$fill
          ),
          alpha = self$theme$aesProperties$alpha[1],
          show.legend = TRUE
        )

        # If no mapping defined, remove dummy aesthetic label from the legend
        plotObject <- plotObject +
          ifEqual("defaultAes", mapLabels$fill, guides(fill = "none"))
      }

      return(plotObject)
    }
  )
)
