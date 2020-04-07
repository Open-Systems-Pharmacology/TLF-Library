#' @title TimeProfilePlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for time profile plots
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @field timeProfileProperties list of properties for time profile plot specific features
    timeProfileProperties = NULL,

    #' @description Create a new \code{TimeProfilePlotConfiguration} object
    #' @param timeProfileProperties list of properties for PK ratio plot specific features
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
    #' @return A new \code{TimeProfilePlotConfiguration} object
    initialize = function(timeProfileProperties = tlfEnv$currentTheme$timeProfile,
                              title = "Time Profile Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )

      self$timeProfileProperties <- timeProfileProperties
    },

    #' @description Add LLOQ limits as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{TimeProfileDataMapping}
    #' @return A \code{ggplot} object with PK ratio limits
    addLLOQLines = function(plotObject, metaData, dataMapping) {
      if (is.null(metaData)) {
        return(plotObject)
      }

      LLOQLines <- ifnotnull(
        dataMapping$y,
        metaData[[dataMapping$y]]$LLOQ,
        metaData[[dataMapping$ymin]]$LLOQ
      )

      if (!is.null(LLOQLines)) {
        for (LLOQIndex in seq(1, length(LLOQLines))) {
          plotObject <- plotObject +
            ggplot2::geom_hline(
              yintercept = LLOQLines[LLOQIndex],
              linetype = self$timeProfileProperties$lloq$linetype[LLOQIndex],
              color = self$timeProfileProperties$lloq$color[LLOQIndex],
              size = self$timeProfileProperties$lloq$size[LLOQIndex]
            )
        }
      }
      return(plotObject)
    },

    #' @description Add time profiles as a combination of scatter, line, ribbon and errorbar layers to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{TimeProfileDataMapping}
    #' @return A \code{ggplot} object
    addTimeProfiles = function(plotObject, data, metaData, dataMapping) {
      # Check if mapping is included in the data
      # Add the group mapping and aesthtics variables in the data.frame
      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      if (!is.null(dataMapping$y)) {
        # Plot error bars if ymin and ymax defined
        if (!is.null(dataMapping$ymin) && !is.null(dataMapping$ymax)) {
          # Shape is not an input of geom_errorbar
          plotObject <- plotObject + ggplot2::geom_errorbar(
            data = mapData,
            mapping = aes_string(
              x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$ymax,
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
            x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$ymax,
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
