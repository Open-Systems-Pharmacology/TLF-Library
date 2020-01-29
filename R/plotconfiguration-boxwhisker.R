#' @title BoxWhiskerPlotConfiguration
#' @description  R6 class defining the configuration of a \code{ggplot} object
#' @export
BoxWhiskerPlotConfiguration <- R6::R6Class(
  "BoxWhiskerPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @description Create a new \code{BoxWhiskerPlotConfiguration} object
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
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @param theme R6 class \code{Theme}
    #' @return A new \code{BoxWhiskerPlotConfiguration} object
    initialize = function(title = "Box Whisker Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )
      # x scale is discrete in box plots
      self$xAxis$scale <- Scaling$discrete
    },

    #' @description Add a boxplot layer to a \code{ggplot} object
    #' @param plotObject a \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{BoxWhiskerDataMapping}
    #' @return A \code{ggplot} object
    addBoxWhisker = function(plotObject, data, metaData, dataMapping) {

      # Get the box plot quantiles from dataMapping
      mapBoxWhiskers <- dataMapping$getBoxWhiskerLimits(data)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + ggplot2::geom_boxplot(
        data = mapBoxWhiskers,
        mapping = aes_string(
          x = mapLabels$x,
          ymin = "ymin",
          lower = "lower",
          middle = "middle",
          upper = "upper",
          ymax = "ymax",
          fill = mapLabels$fill,
          color = mapLabels$color,
          linetype = mapLabels$linetype,
          size = mapLabels$size
        ),
        alpha = self$theme$aesProperties$alpha[1],
        show.legend = TRUE,
        stat = "identity"
      )

      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$fill, guides(fill = "none")) +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$linetype, guides(linetype = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    },

    #' @description Add a outlier points layer to a \code{ggplot} object
    #' @param plotObject a \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{BoxWhiskerDataMapping}
    #' @return A \code{ggplot} object
    addOutliers = function(plotObject, data, metaData, dataMapping) {
      mapOutliers <- dataMapping$getOutliers(data)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject +
        ggplot2::geom_point(
          data = mapOutliers,
          mapping = aes_string(
            x = mapLabels$x,
            y = "maxOutliers",
            group = mapLabels$fill,
            shape = mapLabels$shape,
            color = mapLabels$color,
            size = mapLabels$size
          ),
          show.legend = TRUE,
          na.rm = TRUE,
          position = position_dodge(width = 0.9)
        ) +
        ggplot2::geom_point(
          data = mapOutliers,
          mapping = aes_string(
            x = mapLabels$x,
            y = "minOutliers",
            group = mapLabels$fill,
            shape = mapLabels$shape,
            color = mapLabels$color,
            size = mapLabels$size
          ),
          show.legend = TRUE,
          na.rm = TRUE,
          position = position_dodge(width = 0.9)
        )
      # position = position_dodge(width = 0.9) aligns points with centers of boxplots
      # no matter the number of groups, the value of 0.9 will be always fix,
      # otherwise, points won't be centered anymore

      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$shape, guides(shape = "none")) +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
