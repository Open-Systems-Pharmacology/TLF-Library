#' @title BoxWhiskerPlotConfiguration
#' @description  R6 class defining the configuration of a \code{ggplot} object
#' @export
BoxWhiskerPlotConfiguration <- R6::R6Class(
  "BoxWhiskerPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
    #' @description Create a new \code{BoxWhiskerPlotConfiguration} object
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{BoxWhiskerPlotConfiguration} object
    initialize = function(...) {
      super$initialize(...)
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
        ifEqual("legendLabels", mapLabels$fill, guides(fill = "none")) +
        ifEqual("legendLabels", mapLabels$color, guides(color = "none")) +
        ifEqual("legendLabels", mapLabels$linetype, guides(linetype = "none")) +
        ifEqual("legendLabels", mapLabels$size, guides(size = "none"))

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
        ifEqual("legendLabels", mapLabels$shape, guides(shape = "none")) +
        ifEqual("legendLabels", mapLabels$color, guides(color = "none")) +
        ifEqual("legendLabels", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
