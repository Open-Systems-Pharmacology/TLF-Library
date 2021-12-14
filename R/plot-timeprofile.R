#' @title plotTimeProfile
#' @description
#' Producing Time Profile plots
#'
#' @inheritParams addScatter
#' @param dataMapping 
#' A `TimeProfileDataMapping` object mapping `x`, `y`, `ymin`, `ymax` and aesthetic groups to their variable names of `data`.
#' @param observedData A data.frame to use for plot.
#' Unlike `data`, meant for simulated data, plotted as lines and ribbons;
#' `observedData` is plotted as scatter points and errorbars.
#' @param observedDataMapping 
#' An `ObservedDataMapping` object mapping `x`, `y`, `ymin`, `ymax`, `lloq` and aesthetic groups to their variable names of `observedData`.
#' @param plotConfiguration 
#' An optional `TimeProfilePlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples 
#' # Produce a Time profile plot with observed and simulated data
#' obsData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#' simTime <- seq(1, 10, 0.1)
#' simData <- data.frame(
#' x = simTime, 
#' y = 10*exp(-simTime),
#' ymin = 8*exp(-simTime),
#' ymax = 12*exp(-simTime)
#' )
#' 
#' plotTimeProfile(
#' data = simData, 
#' observedData = obsData,
#' dataMapping = TimeProfileDataMapping$new(x = "x", y = "y", ymin = "ymin", ymax = "ymax"),
#' observedDataMapping = ObservedDataMapping$new(x = "x", y = "y")
#' )
#' 
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
