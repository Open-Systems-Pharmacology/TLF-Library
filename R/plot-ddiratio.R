#' Create a DDI-Ratio plot
#'
#' @title plotDDIRatio
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class PKRatioDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class PKRatioPlotConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @description
#' plotDDIRatio(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
#'
plotDDIRatio <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% DDIRatioDataMapping$new()
  plotConfiguration <- plotConfiguration %||% DDIRatioPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  
  validateIsOfType(dataMapping, DDIRatioDataMapping)
  validateIsOfType(plotConfiguration, DDIRatioPlotConfiguration)
  
  plotObject <- ggplot2::ggplot()
  
  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$setPlotBackground(plotObject)
  plotObject <- plotConfiguration$addDDIRatioLines(plotObject, dataMapping)
  plotObject <- plotConfiguration$addGuestLines(plotObject, dataMapping)
  
  plotObject <- plotConfiguration$addDDIRatios(plotObject, data, metaData, dataMapping)
  
  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  plotObject <- plotConfiguration$setPlotProperties(plotObject)
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)
  
  return(plotObject)
}
