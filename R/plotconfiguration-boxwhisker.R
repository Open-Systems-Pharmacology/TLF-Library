#' @title BoxWhiskerPlotConfiguration
#' @docType class
#' @description  Plot Configuration for Box Whisker Plots
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
        ifEqual("defaultAes", mapLabels$fill, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$linetype, guides(linetype = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    },

    addOutliers = function(plotObject, data, metaData, dataMapping) {
      # TO DO : add method to plot outliers
      mapData <- dataMapping$checkMapData(data, metaData)

      # Get the outliers from data mapping method
      # mapOutliers <- dataMapping$getOutliers(data)
      mapOutliers <- mapData

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + ggplot2::geom_point(
        data = mapOutliers,
        mapping = aes_string(
          x = mapLabels$x,
          y = "outliers",
          shape = mapLabels$shape,
          color = mapLabels$color,
          size = mapLabels$size
        ),
        show.legend = TRUE
      )

      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$shape, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
