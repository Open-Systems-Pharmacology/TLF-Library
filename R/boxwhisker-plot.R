#' @title plotBoxWhisker
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class BoxWhiskerDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class BoxWhiskerConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @param plotObject
#' ggplot object, if null creates new plot, if not add time profile layers to ggplot
#' @description
#' plotBoxWhisker(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
plotBoxWhisker <- function(data,
                           metaData = NULL,
                           dataMapping = NULL,
                           plotConfiguration = NULL,
                           plotObject = NULL) {
  dataMapping <- dataMapping %||% BoxWhiskerDataMapping$new()
  plotConfiguration <- plotConfiguration %||% BoxWhiskerPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, BoxWhiskerDataMapping)
  validateIsOfType(plotConfiguration, BoxWhiskerPlotConfiguration)
  validateIsOfType(plotObject, ggplot, nullAllowed = TRUE)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  
  if(nrow(data)==0){
    warning("No data to plot in BoxWhiskers")
    return(plotObject)
  }

  # Add Plot Configuration layers and box whisker plots
  plotObject <- plotConfiguration$addBoxWhisker(plotObject, data, metaData, dataMapping)
  plotObject <- plotConfiguration$addOutliers(plotObject, data, metaData, dataMapping)

  return(plotObject)
}
