#' @title plotObsVsPred
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' \code{ObsVsPredDataMapping} class or subclass mapping x and y variables to \code{data} variable names.
#' \code{dataMapping} provides also the values of the identity and fold errors lines.
#' This parameter is optional: the \code{tlf} library provides a smart mapping if only \code{data} is provided
#' and default values of the identity and fold errors lines.
#' @param plotConfiguration
#' \code{ObsVsPredConfiguration} class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @param plotObject \code{ggplot} graphical object to which the Observations vs Predictions plot layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add Observations vs Predictions plot layers to a \code{ggplot} graphical object.
#' Identity line is plotted as a diagonal line (TO DO: integrate fold errors lines).
#' Observations vs Predictions are plotted as a scatter plot.
#' @return A \code{ggplot} graphical object
#' @export
plotObsVsPred <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          plotObject = NULL) {
  validateIsOfType(data, "data.frame")
  dataMapping <- dataMapping %||% ObsVsPredDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% ObsVsPredPlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, "ObsVsPredDataMapping")
  validateIsOfType(plotConfiguration, "ObsVsPredPlotConfiguration")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("Obs vs Pred plot"))
    return(plotObject)
  }

  # Get error bars for uncertainty
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    plotObject <- addErrorbar(
      data = data,
      dataMapping = dataMapping,
      plotConfiguration = plotConfiguration,
      plotObject = plotObject
    )
  }
  plotObject <- addScatter(
    data = data,
    dataMapping = dataMapping,
    plotConfiguration = plotConfiguration,
    plotObject = plotObject
  )

  # Add line of identity depending and smoothing
  plotObject <- addObsVsPredLines(
    data = data,
    metaData,
    dataMapping,
    plotConfiguration,
    plotObject
  )
  # LLOQ
  if (!isOfLength(dataMapping$lloq, 0)) {
    # Overwrite y mapping temporarily
    yMapping <- dataMapping$y
    dataMapping$y <- dataMapping$lloq

    plotObject <- addLine(
      data = data,
      dataMapping = dataMapping,
      caption = dataMapping$lloq,
      plotConfiguration = plotConfiguration,
      plotObject = plotObject
    )
    dataMapping$y <- yMapping
  }

  try(suppressMessages(plotObject <- setXAxis(plotObject)))
  try(suppressMessages(plotObject <- setYAxis(plotObject)))
  return(plotObject)
}

#' @title addObsVsPredLines
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' @param dataMapping \code{ObsVsPredDataMapping} object
#' @param plotConfiguration \code{ObsVsPredConfiguration} object
#' @param plotObject \code{ggplot} graphical object
#' @description
#' Add layers of identity line and smoother
#' @return A \code{ggplot} graphical object
addObsVsPredLines <- function(data,
                              metaData = NULL,
                              dataMapping = NULL,
                              plotConfiguration = NULL,
                              plotObject = NULL) {

  # Get range of x values to plot
  xmin <- min(dataMapping$minRange, data[, dataMapping$x], data[, dataMapping$y])
  xmax <- max(dataMapping$minRange, data[, dataMapping$x], data[, dataMapping$y])

  obsVsPredLineData <- data.frame(x = c(xmin, xmax), y = c(xmin, xmax) * dataMapping$lines)
  plotObject <- plotObject +
    ggplot2::geom_path(
      data = obsVsPredLineData,
      mapping = ggplot2::aes_string(x = "x", y = "y"),
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = 0, aesthetic = "color"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = 0, aesthetic = "linetype"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = 0, aesthetic = "size")
    )
  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  if (!isOfLength(dataMapping$smoother, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_smooth(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y),
        method = dataMapping$smoother,
        se = FALSE,
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = 1, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = 1, aesthetic = "linetype"),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = 1, aesthetic = "size")
      )
  }
  return(plotObject)
}

#' @title runObsVsPredPlot
#' @description
#' Run shiny app to use `plotObsVsPred()` from user interface
#' @export
runObsVsPredPlot <- function() {
  appPath <- system.file("obs-vs-pred", package = "tlf")
  shiny::runApp(appPath)
}
