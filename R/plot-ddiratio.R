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
plotDDIRatio <- function(data,
                         metaData = NULL,
                         dataMapping = NULL,
                         plotConfiguration = NULL,
                         plotObject = NULL) {
  dataMapping <- dataMapping %||% DDIRatioDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% DDIRatioPlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, DDIRatioDataMapping)
  validateIsOfType(plotConfiguration, DDIRatioPlotConfiguration)

  ratioData <- dataMapping$getDDIRatioLines()
  guestData <- dataMapping$getGuestLines()

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  plotObject <- addLine(x = ratioData$x, y = ratioData$y, caption = "ddiRatioLine1", plotObject = plotObject)
  plotObject <- addLine(x = ratioData$x, y = ratioData$ymin, caption = "ddiRatioLine2", plotObject = plotObject)
  plotObject <- addLine(x = ratioData$x, y = ratioData$ymax, caption = "ddiRatioLine2", plotObject = plotObject)

  plotObject <- addLine(x = ratioData$x, y = guestData$ymin, caption = "guestLine", plotObject = plotObject)
  plotObject <- addLine(x = ratioData$x, y = guestData$ymax, caption = "guestLine", plotObject = plotObject)

  plotObject <- setLegendCaption(plotObject, plotConfiguration$ddiRatioCaption)
  plotObject <- addScatter(data = data, dataMapping = dataMapping, plotObject = plotObject)
  return(plotObject)
}
