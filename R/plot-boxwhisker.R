#' Create a Box Whisker plot
#'
#' @title plotBoxWhisker
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class BoxWhiskerDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class BoxWhiskerConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @description
#' plotBoxWhisker(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
#'
plotBoxWhisker <- function(data,
                           metaData = NULL,
                           dataMapping = NULL,
                           plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% BoxWhiskerDataMapping$new()
  plotConfiguration <- plotConfiguration %||% BoxWhiskerPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, BoxWhiskerDataMapping)
  validateIsOfType(plotConfiguration, BoxWhiskerPlotConfiguration)

  plotObject <- ggplot2::ggplot()

  # Add Plot Configuration layers and box whisker plots
  plotObject <- plotConfiguration$setPlotBackground(plotObject)
  plotObject <- plotConfiguration$addBoxWhisker(plotObject, data, metaData, dataMapping)
  # TO DO: create addOutliers method
  # plotObject <- plotConfiguration$addOutliers(plotObject, data, metaData, dataMapping)

  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  # TO DO: fix x axis which has become discrete, scale_x_continous crashes
  # plotObject <- plotConfiguration$setPlotProperties(plotObject)
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)

  return(plotObject)
}
