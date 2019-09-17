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
plotTimeProfile <- function(data, metaData, dataMapping = NULL, plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  configuration <- plotConfiguration %||% TimeProfilePlotConfiguration$new()
  dataMapping <- dataMapping %||% TimeProfileDataMapping$new()

  stopifnot("TimeProfileDataMapping" %in% class(dataMapping))
  stopifnot("TimeProfilePlotConfiguration" %in% class(configuration))

  # data expected as list of data.frame
  if ("data.frame" %in% class(data)) {
    data <- list(data)
  }

  observationSets <- ifnotnull(
    dataMapping$observationSets,
    data[[dataMapping$observationSets]], NULL
  )
  simulationSets <- ifnotnull(
    dataMapping$simulationSets,
    data[[dataMapping$simulationSets]], NULL
  )

  simulationMapping <- dataMapping$simulationMapping
  observationMapping <- dataMapping$observationMapping

  xSim <- simulationSets[, simulationMapping$x]
  ySim <- simulationSets[, simulationMapping$y]

  xObs <- observationSets[, observationMapping$x]
  yObs <- observationSets[, observationMapping$y]
  errorPlot <- FALSE

  if ("XYEDataMapping" %in% class(observationMapping)) {
    errorPlot <- TRUE
    yObsMin <- observationSets[, observationMapping$y] - observationSets[, observationMapping$errorMin]
    yObsMax <- observationSets[, observationMapping$y] + observationSets[, observationMapping$errorMax]
  }

  # Initialize plot
  plotObject <- ggplot2::ggplot()
  plotObject <- plotConfiguration$setWatermark(plotObject)
  plotObject <- plotConfiguration$setPlotLabels(plotObject, dataMapping$simulationMapping)

  # Add simulations
  plotObject <- plotObject + ggplot2::geom_line(
    data = simulationSets,
    mapping = aes(x = xSim, y = ySim, linetype = factor("Simulated Results")), show.legend = TRUE
  )
  # Add observations
  plotObject <- plotObject +
    ifnotnull(
      observationSets,
      if (errorPlot) {
        ggplot2::geom_pointrange(
          data = observationSets,
          mapping = aes(x = xObs, y = yObs, ymin = yObsMin, ymax = yObsMax, shape = factor("Observed Data")), show.legend = TRUE
        )
      } else {
        ggplot2::geom_point(
          data = observationSets,
          mapping = aes(x = xObs, y = yObs, shape = factor("Observed Data")), show.legend = TRUE
        )
      }
    )

  # Add Plot Configuration layers and PK Ratios
  return(plotObject)
}
