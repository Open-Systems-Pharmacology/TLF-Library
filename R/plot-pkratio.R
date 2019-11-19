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
plotPKRatio <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()
  plotConfiguration <- plotConfiguration %||% PKRatioPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, PKRatioDataMapping)
  validateIsOfType(plotConfiguration, PKRatioPlotConfiguration)

  # mapData <- dataMapping$getMapData(data, metaData)

  plotObject <- ggplot2::ggplot()

  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$setPlotBackground(plotObject)
  plotObject <- plotConfiguration$addPKRatioLines(plotObject, dataMapping$pkRatioLines)

  plotObject <- plotConfiguration$addPKRatios(plotObject, data, metaData, dataMapping)

  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  plotObject <- plotConfiguration$setPlotProperties(plotObject)
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)

  return(plotObject)
}
