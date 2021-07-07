#' @title plotTimeProfile
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list containing complementary information to data (e.g. unit)
#' @param dataMapping `TimeProfileDataMapping` object
#' @param observedData data.frame containing the data to be used for the plot
#' @param observedDataMapping `ObservedDataMapping` object
#' @param plotConfiguration `TimeProfilePlotConfiguration` object
#' @param plotObject
#' ggplot object, if null creates new plot, if not add time profile layers to ggplot
#' @description
#' plotTimeProfile(data, metaData = NULL, dataMapping = NULL, plotConfiguration = NULL, plotObject = NULL)
#' @return a ggplot graphical object
#' @export
plotTimeProfile <- function(data = NULL,
                            metaData = NULL,
                            dataMapping = NULL,
                            observedData = NULL,
                            observedDataMapping = NULL,
                            plotConfiguration = NULL,
                            plotObject = NULL) {
  validateIsOfType(data, "data.frame", nullAllowed = TRUE)
  validateIsOfType(observedData, "data.frame", nullAllowed = TRUE)
  if (all(isOfLength(data, 0), isOfLength(observedData, 0))) {
    warning("data and observed data are of length 0. Time profile layer was not added.")
    return(plotObject)
  }

  if (!isOfLength(data, 0)) {
    dataMapping <- dataMapping %||% TimeProfileDataMapping$new(data = data)
  }
  if (!isOfLength(observedData, 0)) {
    observedDataMapping <- observedDataMapping %||% ObservedDataMapping$new(data = data)
  }

  plotConfiguration <- plotConfiguration %||% TimeProfilePlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, TimeProfileDataMapping, nullAllowed = TRUE)
  validateIsOfType(observedDataMapping, ObservedDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, TimeProfilePlotConfiguration)

  # Initialize plot based on plotConfiguration
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  if (!isOfLength(data, 0)) {
    mapData <- dataMapping$checkMapData(data)
    if (!any(isOfLength(dataMapping$ymin, 0), isOfLength(dataMapping$ymax, 0))) {
      plotObject <- addRibbon(
        data = mapData,
        dataMapping = dataMapping,
        plotConfiguration = plotConfiguration,
        plotObject = plotObject
      )
    }
    if (!isOfLength(dataMapping$y, 0)) {
      plotObject <- addLine(
        data = mapData,
        dataMapping = dataMapping,
        plotConfiguration = plotConfiguration,
        plotObject = plotObject
      )
    }
  }

  if (!isOfLength(observedData, 0)) {
    mapObservedData <- observedDataMapping$checkMapData(observedData)
    if (!isOfLength(observedDataMapping$uncertainty, 0)) {
      plotObject <- addErrorbar(
        data = mapObservedData,
        dataMapping = observedDataMapping,
        plotConfiguration = plotConfiguration,
        plotObject = plotObject
      )
    }
    plotObject <- addScatter(
      data = mapObservedData,
      dataMapping = observedDataMapping,
      plotConfiguration = plotConfiguration,
      plotObject = plotObject
    )
    # LLOQ
    if (!isOfLength(observedDataMapping$lloq, 0)) {
      mapObservedData$legendLabels <- paste(mapObservedData$legendLabels, " LLOQ")
      plotObject <- addLine(
        data = mapObservedData,
        dataMapping = XYGDataMapping$new(
          x = observedDataMapping$x,
          y = "lloq",
          color = "legendLabels"
        ),
        plotConfiguration = plotConfiguration,
        plotObject = plotObject
      )
    }
  }
  return(plotObject)
}
