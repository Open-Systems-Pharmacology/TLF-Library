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
                          bins = NULL,
                          stack = NULL,
                          fitNormalDist = NULL,
                          fitDensity = NULL,
                          plotConfiguration = NULL,
                          plotObject = NULL) {
  dataMapping <- dataMapping %||% HistogramDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% HistogramPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, "HistogramDataMapping")
  validateIsOfType(plotConfiguration, "HistogramPlotConfiguration")
  validateIsOfType(data, "data.frame")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  # Overwrites plotConfiguration and dataMapping if some inputs are not null
  validateIsLogical(stack, nullAllowed = TRUE)
  validateIsLogical(fitNormalDist, nullAllowed = TRUE)
  validateIsLogical(fitDensity, nullAllowed = TRUE)
  dataMapping$stack <- stack %||% dataMapping$stack
  dataMapping$fitNormalDist <- fitNormalDist %||% dataMapping$fitNormalDist
  dataMapping$fitDensity <- fitDensity %||% dataMapping$fitDensity
  dataMapping$bins <- bins %||% dataMapping$bins

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("Histogram"))
    return(plotObject)
  }

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  position <- ggplot2::position_nudge()
  if (dataMapping$stack) {
    position <- ggplot2::position_stack()
  }

  plotObject <- plotObject +
    ggplot2::geom_histogram(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        fill = mapLabels$fill
      ),
      position = position,
      bins = dataMapping$bins %||% tlfEnv$defaultAggregation$bins,
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$size, position = 0, aesthetic = "size"),
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$color, position = 0, aesthetic = "color"),
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$alpha, position = 0, aesthetic = "alpha")
    )

  # Vertical lines defined in dataMapping$lines
  for (lineIndex in dataMapping$lines) {
    plotObject <- plotObject +
      ggplot2::geom_vline(
        xintercept = dataMapping$lines[lineIndex],
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex - 1, aesthetic = "size"),
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex - 1, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex - 1, aesthetic = "linetype")
      )
  }
  # Lines fitting a normal distribution
  if (dataMapping$fitNormalDist) {
    histResult <- graphics::hist(data[, dataMapping$x], breaks = dataMapping$bins %||% tlfEnv$defaultAggregation$bins, plot = FALSE)
    scalingFactor <- mean(histResult$counts[histResult$counts > 0] / histResult$density[histResult$counts > 0])
    xmean <- mean(data[, dataMapping$x])
    xsd <- stats::sd(data[, dataMapping$x], na.rm = TRUE)
    xDensityData <- seq(xmean - 3 * xsd, xmean + 3 * xsd, 6 * xsd / 100)
    yDensityData <- scalingFactor * stats::dnorm(xDensityData, mean = xmean, sd = xsd)
    densityData <- data.frame(x = xDensityData, y = yDensityData)

    plotObject <- plotObject +
      ggplot2::geom_path(
        data = densityData,
        mapping = ggplot2::aes_string(x = "x", y = "y"),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = 0, aesthetic = "size"),
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = 0, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = 0, aesthetic = "linetype")
      )
  }

  # Define fill based on plotConfiguration$points properties
  fillVariable <- gsub("`", "", mapLabels$fill)
  fillLength <- length(unique(mapData[, fillVariable]))

  plotObject <- plotObject +
    ggplot2::scale_fill_manual(values = getAestheticValues(n = fillLength, selectionKey = plotConfiguration$ribbons$fill, aesthetic = "fill"))

  # If variable is legendLabel, remove it from legend
  if (isIncluded(fillVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(fill = FALSE)
  }
  # dataMapping$smoother
  return(plotObject)
}
