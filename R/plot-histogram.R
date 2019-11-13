#' Create a histogram
#'
#' @title plotHistogram
#' @param data data.frames containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class HistogramDataMapping
#' mapping of which data to use for histogram
#' @param plotConfiguration R6 class HistogramPlotConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @param binWidth (optional)
#' @param bins (optional)
#' @param verticalLineFunctions (optional)
#' @param verticalLineFunctionNames (optional)
#' @description
#' plotHistogram(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
#'
plotHistogram <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          binWidth = NULL,
                          bins = NULL,
                          verticalLineFunctions = NULL,
                          verticalLineFunctionNames = NULL) {

  # If no data mapping or plot configuration is input, use default
  metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% HistogramDataMapping$new()
  plotConfiguration <- plotConfiguration %||% HistogramPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping,
    binWidth = binWidth,
    bins = bins,
    verticalLineFunctions = verticalLineFunctions,
    verticalLineFunctionNames = verticalLineFunctionNames
  )

  validateIsOfType(dataMapping, HistogramDataMapping)
  validateIsOfType(plotConfiguration, HistogramPlotConfiguration)

  plotObject <- ggplot2::ggplot()
  plotObject <- plotConfiguration$setPlotBackground(plotObject)
  plotObject <- plotConfiguration$addHistograms(plotObject, data = data, metaData = metaData, dataMapping = dataMapping, bins = bins, binWidth = binWidth)
  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)
  plotObject <- plotConfiguration$addVerticalLines(plotObject)

  return(plotObject)
}
