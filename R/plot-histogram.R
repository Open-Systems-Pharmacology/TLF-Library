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
#' @param plotObject
#' ggplot object, if null creates new plot, if not add time profile layers to ggplot
#' @description
#' plotHistogram(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
plotHistogram <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          binWidth = NULL,
                          bins = NULL,
                          verticalLineFunctions = NULL,
                          verticalLineFunctionNames = NULL,
                          plotObject = NULL) {
  dataMapping <- dataMapping %||% HistogramDataMapping$new()
  plotConfiguration <- plotConfiguration %||% HistogramPlotConfiguration$new(
    binWidth = binWidth,
    bins = bins,
    ylabel = "Count",
    data = data,
    metaData = metaData,
    dataMapping = dataMapping)

  validateIsOfType(dataMapping, HistogramDataMapping)
  validateIsOfType(plotConfiguration, HistogramPlotConfiguration)
  validateIsOfType(plotObject, ggplot, nullAllowed = TRUE)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  plotObject <- plotConfiguration$addHistograms(plotObject, data, metaData, dataMapping, bins, binWidth)
  
  #plotObject <- plotConfiguration$addVerticalLines(plotObject, data, metaData, dataMapping)

  return(plotObject)
}
