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
                         comparisonType = NULL,
                         plotConfiguration = NULL,
                         plotObject = NULL) {
  validateIsOfType(data, "data.frame")
  dataMapping <- dataMapping %||% DDIRatioDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% DDIRatioPlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, "DDIRatioDataMapping")
  validateIsOfType(plotConfiguration, "PKRatioPlotConfiguration")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("PK ratio plot"))
    return(plotObject)
  }

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  plotObject <- addDDIRatioLines(
    data,
    metaData,
    dataMapping,
    plotConfiguration = plotConfiguration,
    plotObject = plotObject
  )

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_linerange(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          ymin = "ymin",
          ymax = "ymax",
          color = mapLabels$color
        ),
        # Error bar size uses a ratio of 1/4 to match with point size
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype"),
        show.legend = TRUE
      )
  }
  plotObject <- plotObject +
    ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        color = mapLabels$color,
        shape = mapLabels$shape
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      show.legend = TRUE
    )

  # Define shapes and colors based on plotConfiguration$points properties
  shapeVariable <- gsub("`", "", mapLabels$shape)
  colorVariable <- gsub("`", "", mapLabels$color)
  shapeLength <- length(unique(mapData[, shapeVariable]))
  colorLength <- length(unique(mapData[, colorVariable]))

  plotObject <- plotObject +
    ggplot2::scale_shape_manual(values = getAestheticValues(n = shapeLength, selectionKey = plotConfiguration$points$shape, aesthetic = "shape")) +
    ggplot2::scale_color_manual(values = getAestheticValues(n = colorLength, selectionKey = plotConfiguration$points$color, aesthetic = "color"))

  # If variable is legendLabel, remove it from legend
  if (isIncluded(shapeVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(shape = FALSE)
  }
  if (isIncluded(colorVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(color = FALSE)
  }
  # Try is used to prevent crashes in the final plot due to ggplot2 peculiarities regarding scale functions
  try(suppressMessages(plotObject <- setXAxis(plotObject)))
  try(suppressMessages(plotObject <- setYAxis(plotObject)))
  return(plotObject)
}

#' @title addDDIRatioLines
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class PKRatioDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration R6 class PKRatioPlotConfiguration
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @description
#' addDDIRatioLines(data, metaData, dataMapping, plotConfiguration, plotObject)
#' @return a ggplot graphical object
addDDIRatioLines <- function(data,
                             metaData = NULL,
                             dataMapping = NULL,
                             comparisonType = NULL,
                             plotConfiguration = NULL,
                             plotObject = NULL) {
  validateIsOfType(dataMapping, "DDIRatioDataMapping")
  validateIsOfType(plotConfiguration, "PKRatioPlotConfiguration")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)
  validateIsIncluded(comparisonType, DDIComparisonTypes, nullAllowed = TRUE)

  dataMapping$comparisonType <- comparisonType %||% dataMapping$comparisonType

  # Get range of x values to plot
  xmin <- min(dataMapping$minRange, data[, dataMapping$x])
  xmax <- max(dataMapping$minRange, data[, dataMapping$x])
  # Not sure yet if this should be handled this way or using an external option from the dataMapping
  x <- 10^(seq(log10(xmin), log10(xmax), 1e-3))

  ratioData <- list()

  # If comparison type is obs vs pred: to be compared with line of identity
  if (isIncluded(dataMapping$comparisonType, DDIComparisonTypes$obsVsPred)) {
    ratioData$ddiRatio1 <- data.frame(
      x = x,
      y = dataMapping$lines$ddiRatio1 * x
    )
    ratioData$ddiRatio2 <- data.frame(
      x = x,
      ymin = min(dataMapping$lines$ddiRatio2) * x,
      ymax = max(dataMapping$lines$ddiRatio2) * x
    )
    ratioData$guestRatio <- getGuestValues(x, delta = dataMapping$lines$guestRatio)
  }
  # If comparison type is residuals vs pred: to be compared with line of ratio = 1
  if (isIncluded(dataMapping$comparisonType, DDIComparisonTypes$resVsPred)) {
    ratioData$ddiRatio1 <- data.frame(
      x = x,
      y = dataMapping$lines$ddiRatio1
    )
    ratioData$ddiRatio2 <- data.frame(
      x = x,
      ymin = min(dataMapping$lines$ddiRatio2),
      ymax = max(dataMapping$lines$ddiRatio2)
    )
    ratioData$guestRatio <- getGuestValues(x, delta = dataMapping$lines$guestRatio)
    ratioData$guestRatio$ymin <- ratioData$guestRatio$ymin / ratioData$guestRatio$x
    ratioData$guestRatio$ymax <- ratioData$guestRatio$ymax / ratioData$guestRatio$x
  }

  # Use expressions to simplify code
  # Goal is to draw ratio lines with specific aesthetic values
  ratioLineNames <- c("ddiRatio1", "ddiRatio2", "ddiRatio2", "guestRatio", "guestRatio")
  ratioLineVariables <- c("y", "ymin", "ymax", "ymin", "ymax")
  ratioLinePositions <- c("0", "1", "1", "2", "2")

  addLineExpression <- parse(text = paste0("plotObject <- plotObject + 
                                           ggplot2::geom_path(data = ratioData$", ratioLineNames, ", 
                                           mapping = ggplot2::aes_string(x='x', y='", ratioLineVariables, "'),
                                           color = getAestheticValues(n=1, selectionKey = plotConfiguration$lines$color, position = ", ratioLinePositions, ", aesthetic = 'color'), 
                                           linetype = getAestheticValues(n=1, selectionKey = plotConfiguration$lines$linetype, position = ", ratioLinePositions, ", aesthetic = 'linetype'), 
                                           size = getAestheticValues(n=1, selectionKey = plotConfiguration$lines$size, position = ", ratioLinePositions, ", aesthetic = 'size'))"))
  eval(addLineExpression)
  return(plotObject)
}

#' @title getGuestValues
#' @description Get a data.frame with Guest et al. ratio limits
#' @param x input values of Guest function
#' @param delta parameter of Guest function
#' @return A data.frame with Guest et al. ratio limits as ymin and ymax
#' @export
getGuestValues <- function(x, delta = 1) {
  xSym <- x
  xSym[x < 1] <- 1 / x[x < 1]
  limit <- (delta + 2 * (xSym - 1)) / xSym
  ymin <- x / limit
  ymax <- x * limit

  guestLines <- data.frame(x, ymin, ymax)
  return(guestLines)
}