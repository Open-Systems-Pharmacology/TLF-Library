#' @title plotObsVsPred
#' @description
#' Producing observed vs predicted plots
#'
#' @inheritParams addScatter
#' @param smoother Optional name of smoother function: 
#' \itemize{
#' \item `"loess"` for loess regression
#' \item `"lm"` for linear regression
#' }
#' @param dataMapping 
#' A `ObsVsPredDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration 
#' An optional `ObsVsPredConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples 
#' # Produce Obs vs Pred plot
#' obsVsPredData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#' 
#' plotObsVsPred(data = obsVsPredData, dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"))
#' 
#' # Produce Obs vs Pred plot with linear regression
#' plotObsVsPred(
#' data = obsVsPredData, 
#' dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
#' smoother = "lm"
#' )
#' 
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
        na.rm = TRUE,
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
        na.rm = TRUE,
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
