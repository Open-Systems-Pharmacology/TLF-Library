#' Create a Time Profile plot
#'
#' @title plotTimeProfile
#' @param data list of data.frames (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class TimeProfileDataMapping
#' mapping of which data is observation, which data is simulation
#' x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class PKRatioPlotConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @description
#' plotTimeProfile(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
#'
#'
plotTimeProfile <- function(data,
                            metaData = NULL,
                            dataMapping = NULL,
                            plotConfiguration = NULL) {

  # If no data mapping or plot configuration is input, use default
  metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% TimeProfileDataMapping$new()
  plotConfiguration <- plotConfiguration %||% TimeProfilePlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  stopifnot("TimeProfileDataMapping" %in% class(dataMapping))
  stopifnot("TimeProfilePlotConfiguration" %in% class(plotConfiguration))


  plotObject <- ggplot2::ggplot()

  plotObject <- plotConfiguration$setWatermark(plotObject)

  plotObject <- plotConfiguration$addLLOQLines(dataMapping$LLOQ, plotObject)

  plotObject <- plotConfiguration$addTimeProfiles(plotObject, data, metaData, dataMapping)

  plotObject <- plotConfiguration$setPlotLabels(plotObject)
  plotObject <- plotConfiguration$setPlotProperties(plotObject)
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)

  return(plotObject)
}
