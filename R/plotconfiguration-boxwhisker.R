#' @title BoxWhiskerPlotConfiguration
#' @docType class
#' @description  Class for BoxWhisker Plot Configuration
#' @field legend R6 class defining legendConfiguration
#' @field xAxis R6 class defining xAxisConfiguration
#' @field yAxis R6 class defining yAxisConfiguration
#' @field background R6 class defining backgroundConfiguration
#' @field theme R6 class defining theme aesthtic properties
#' @field filename Name of the saved plot
#' @section Methods:
#' \describe{
#' \item{new(...)}{Initialize BoxWhiskerPlotConfiguration}
#' \item{setPlotProperties(plotObject)}{Apply properties of plot labels.}
#' \item{setPlotBackground(plotObject)}{Apply background properties to plot.}
#' \item{savePlot(plotObject)}{Save ggplot as file.}
#' \item{addBoxWhisker(plotObject, data, metaData, dataMapping)}{Add Box and whiskers to plot.}
#' \item{addOutliers(plotObject, data, metaData, dataMapping)}{Add Outliers to plot.}
#' }
#' @export
BoxWhiskerPlotConfiguration <- R6::R6Class(
  "BoxWhiskerPlotConfiguration",
  inherit = PlotConfiguration,

  public = list(
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
