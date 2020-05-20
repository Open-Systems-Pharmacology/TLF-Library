#' @title plotTimeProfile
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list containing complementary information to data (e.g. unit)
#' @param dataMapping `TimeProfileDataMapping` object
#' @param plotConfiguration `TimeProfilePlotConfiguration` object
#' @param plotObject
#' ggplot object, if null creates new plot, if not add time profile layers to ggplot
#' @description
#' plotTimeProfile(data, metaData = NULL, dataMapping = NULL, plotConfiguration = NULL, plotObject = NULL)
#' @return a ggplot graphical object
#' @export
plotTimeProfile <- function(data,
                            metaData = NULL,
                            dataMapping = NULL,
                            observedData = NULL,
                            dataMappingForObserved = NULL,
                            plotConfiguration = NULL,
                            plotObject = NULL) {
  dataMapping <- dataMapping %||% TimeProfileDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% TimeProfilePlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(data, "data.frame")
  validateIsOfType(dataMapping, TimeProfileDataMapping)
  validateIsOfType(plotConfiguration, TimeProfilePlotConfiguration)
  validateIsOfType(observedData, "data.frame", nullAllowed = TRUE)
  validateIsOfType(dataMappingForObserved, XYGDataMapping, nullAllowed = TRUE)

  lloq <- dataMapping$lloq
  isRangeTimeProfile <- dataMapping$isRangeTimeProfile

  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  if (!is.null(lloq)) {
    plotObject <- addLine(y = lloq, caption = "lloq", plotObject = plotObject)
    plotObject <- setLegendCaption(plotObject, plotConfiguration$timeProfileCaption)
  }
  if (isRangeTimeProfile) {
    aggregatedData <- getAggregatedData(dataMapping$checkMapData(data), dataMapping$x, dataMapping$y)
    rangeData <- aggregatedData
    rangeData$Groups <- paste(tlfEnv$defaultAggregation$labels$range, aggregatedData$Groups)
    rangeMapping <- RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax", color = "Groups")
    plotObject <- addRibbon(data = rangeData, dataMapping = rangeMapping, plotObject = plotObject)
    lineMapping <- XYGDataMapping$new(x = "x", y = "y", color = "Groups")
    lineData <- aggregatedData
    lineData$Groups <- paste(tlfEnv$defaultAggregation$labels$y, aggregatedData$Groups)
    plotObject <- addLine(data = lineData, dataMapping = lineMapping, plotObject = plotObject)
  }
  if (!isRangeTimeProfile) {
    plotObject <- addLine(data = data, dataMapping = dataMapping, plotObject)
  }
  if (!is.null(observedData)) {
    dataMappingForObserved <- dataMappingForObserved %||% XYGDataMapping$new(data = observedData)
    plotObject <- addScatter(data = observedData, dataMapping = dataMappingForObserved, plotObject = plotObject)
  }

  return(plotObject)
}
