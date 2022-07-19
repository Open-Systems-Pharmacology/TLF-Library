#' @title plotObsVsPred
#' @description
#' Producing observed vs predicted plots
#'
#' @inheritParams addScatter
#' @param foldDistance Numeric values of fold distance lines to display in log plots.
#' This argument is internally translated into `lines` field of `dataMapping`.
#' __Caution__: this argument is meant for log scaled plots and since fold distance is a ratio it is expected positive.
#' In particular, line of identity corresponds to a `foldDistance` of `1`.
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
#'   data = obsVsPredData,
#'   dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
#'   smoother = "lm"
#' )
#'
#' # Produce Obs vs Pred plot with user-defined fold distance lines
#' plotObsVsPred(
#'   data = obsVsPredData,
#'   dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
#'   plotConfiguration = ObsVsPredPlotConfiguration$new(
#'     xScale = Scaling$log, xLimits = c(0.05, 50),
#'     yScale = Scaling$log, yLimits = c(0.05, 50)
#'   ),
#'   foldDistance = c(1, 10)
#' )
#'
plotObsVsPred <- function(data,
                          metaData = NULL,
                          dataMapping = NULL,
                          plotConfiguration = NULL,
                          foldDistance = NULL,
                          smoother = NULL,
                          plotObject = NULL) {
  eval(.parseCheckPlotInputs("ObsVsPred"))
  validateIsIncluded(smoother, c("loess", "lm"), nullAllowed = TRUE)
  validateIsNumeric(foldDistance, nullAllowed = TRUE)
  dataMapping$smoother <- smoother %||% dataMapping$smoother
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- .getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Add diagonal lines with offset defined in lines of dataMapping
  if (!isEmpty(foldDistance)) {
    dataMapping$lines <- getLinesFromFoldDistance(foldDistance)
  }
  for (lineIndex in seq_along(dataMapping$lines)) {
    lineValue <- .getAblineValues(dataMapping$lines[[lineIndex]], plotConfiguration$yAxis$scale)
    # position correspond to the number of layer lines already added
    eval(.parseAddLineLayer("diagonal", lineValue, lineIndex - 1))
  }
  if (isEmpty(lineIndex)) {
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
        color = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
        linetype = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
        alpha = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
        size = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
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
        color = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
        linetype = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
        alpha = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
        size = .getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
      )
  }

  eval(.parseAddUncertaintyLayer(direction = "horizontal"))
  eval(.parseAddScatterLayer())
  # Define shapes and colors based on plotConfiguration$points properties
  eval(.parseUpdateAestheticProperty(AestheticProperties$color, "points"))
  eval(.parseUpdateAestheticProperty(AestheticProperties$shape, "points"))
  eval(.parseUpdateAxes())
  return(plotObject)
}
