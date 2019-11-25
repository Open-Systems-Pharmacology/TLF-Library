#' @title TimeProfilePlotConfiguration
#' @docType class
#' @description  Class for TimeProfile Plot Configuration
#' @field legend R6 class defining legendConfiguration
#' @field xAxis R6 class defining xAxisConfiguration
#' @field yAxis R6 class defining yAxisConfiguration
#' @field background R6 class defining backgroundConfiguration
#' @field theme R6 class defining theme aesthtic properties
#' @field filename Name of the saved plot
#' @field timeProfileProperties Properties of time profile plot specific features
#' @section Methods:
#' \describe{
#' \item{new(timeProfileProperties = tlfEnv$currentTheme$timeProfile, title = "Time Profile Plot", subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")), ...)}{
#' Initialize TimeProfilePlotConfiguration}
#' \item{setPlotProperties(plotObject)}{Apply properties of plot labels.}
#' \item{setPlotBackground(plotObject)}{Apply background properties to plot.}
#' \item{savePlot(plotObject)}{Save ggplot as file.}
#' \item{addLLOQLines(plotObject, metaData, dataMapping)}{Add LLOQ horizontal lines to plot.}
#' \item{addTimeProfiles(plotObject, data, metaData, dataMapping)}{Add time profiles to plot.}
#' }
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    timeProfileProperties = NULL,

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
