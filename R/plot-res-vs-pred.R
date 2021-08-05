#' @title plotResVsPred
#' @inheritParams plotObsVsPred
#' @description
#' Add Residuals vs Predictions / Time plot layers to a \code{ggplot} graphical object.
#' Residuals vs Predictions are plotted as a scatter plot.
#' @return A \code{ggplot} graphical object
#' @export
plotResVsPred <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          smoother = NULL,
                          plotObject = NULL) {
  eval(parseCheckPlotInputs("ResVsPred"))
  validateIsIncluded(smoother, c("loess", "lm"), nullAllowed = TRUE)
  dataMapping$smoother <- smoother %||% dataMapping$smoother
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  
  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  
  for (lineIndex in seq_along(dataMapping$lines)) {
    eval(parseAddLineLayer("horizontal", dataMapping$lines[[lineIndex]], lineIndex - 1))
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
  eval(parseUpdateAxes())
  return(plotObject)
}
