#' Create a PK-Ratio plot
#'
#' @title plotPKRatio
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class PKRatioDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class PKRatioPlotConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @description
#' plotPKRatio(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
#'
plotPKRatio <- function(data, metaData, dataMapping = NULL, plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  configuration <- plotConfiguration %||% PKRatioPlotConfiguration$new()
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))
  stopifnot("PKRatioPlotConfiguration" %in% class(configuration))

  mapData <- dataMapping$getMapData(data, metaData)

  plotObject <- ggplot2::ggplot()

  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$setWatermark(plotObject)
  plotObject <- plotConfiguration$addPKRatioLines(dataMapping$pkRatioLines, plotObject)

  plotObject <- plotConfiguration$addPKRatios(plotObject, data, metaData, dataMapping)

  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  plotObject <- plotConfiguration$setPlotProperties(plotObject)

  return(plotObject)
}
