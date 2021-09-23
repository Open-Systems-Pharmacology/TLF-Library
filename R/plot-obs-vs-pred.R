#' @title plotObsVsPred
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' `ObsVsPredDataMapping` class or subclass mapping x and y variables to `data` variable names.
#' `dataMapping` provides also the values of the identity and fold errors lines.
#' This parameter is optional: the `tlf` library provides a smart mapping if only `data` is provided
#' and default values of the identity and fold errors lines.
#' @param plotConfiguration
#' `ObsVsPredConfiguration` class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the `tlf` library provides a default configuration according to the current theme
#' @param smoother smoother Name of smoother function: "loess" or "lm"
#' @param plotObject `ggplot` graphical object to which the Observations vs Predictions plot layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add Observations vs Predictions plot layers to a `ggplot` graphical object.
#' Observations vs Predictions are plotted as a scatter plot.
#' @return A `ggplot` graphical object
#' @export
plotObsVsPred <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          smoother = NULL,
                          plotObject = NULL) {
  eval(parseCheckPlotInputs("ObsVsPred"))
  validateIsIncluded(smoother, c("loess", "lm"), nullAllowed = TRUE)
  dataMapping$smoother <- smoother %||% dataMapping$smoother
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  for (lineIndex in seq_along(dataMapping$lines)) {
    eval(parseAddLineLayer("diagonal", dataMapping$lines[[lineIndex]], lineIndex - 1))
  }
  if (isOfLength(lineIndex, 0)) {
    lineIndex <- 0
  }
  # Add Smoother if defined
  if (isIncluded(dataMapping$smoother, "loess")) {
    plotObject <- plotObject +
      ggplot2::geom_smooth(
        data = mapData,
        method = "loess",
        se = FALSE,
        formula = "y ~ x",
        mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y),
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
      )
  }
  if (isIncluded(dataMapping$smoother, "lm")) {
    plotObject <- plotObject +
      ggplot2::geom_smooth(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y),
        method = "lm",
        se = FALSE,
        formula = "y ~ x",
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
      )
  }

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    eval(parseAddUncertaintyLayer())
  }
  eval(parseAddScatterLayer())
  # Define shapes and colors based on plotConfiguration$points properties
  eval(parseUpdateAestheticProperty(AestheticProperties$color, "points"))
  eval(parseUpdateAestheticProperty(AestheticProperties$shape, "points"))
  plotObject <- setLegendPosition(plotObject)
  plotObject <- setLegendFont(plotObject)
  eval(parseUpdateAxes())
  return(plotObject)
}
