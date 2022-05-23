#' @title plotResVsPred
#' @description
#' Producing residuals vs predicted plots
#'
#' @inheritParams plotObsVsPred
#' @param dataMapping 
#' A `ResVsPredDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration 
#' An optional `ResVsPredConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce Obs vs Pred plot
#' resVsPredData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#'
#' plotResVsPred(data = resVsPredData, dataMapping = ResVsPredDataMapping$new(x = "x", y = "y"))
#'
#' # Produce Res vs Pred plot with linear regression
#' plotResVsPred(
#'   data = resVsPredData,
#'   dataMapping = ResVsPredDataMapping$new(x = "x", y = "y"),
#'   smoother = "lm"
#' )
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

  # Add horizontal lines with offset defined in lines of dataMapping
  for (lineIndex in seq_along(dataMapping$lines)) {
    eval(parseAddLineLayer("horizontal", dataMapping$lines[[lineIndex]], lineIndex - 1))
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
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
        alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
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
        alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
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


#' @title plotResVsTime
#' @description
#' Producing residuals vs time plots
#'
#' @inheritParams plotObsVsPred
#' @param dataMapping 
#' A `ResVsTimeDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration 
#' An optional `ResVsTimeConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples 
#' # Produce Obs vs Pred plot
#' resVsTimeData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#' 
#' plotResVsTime(data = resVsTimeData, dataMapping = ResVsTimeDataMapping$new(x = "x", y = "y"))
#' 
#' # Produce Res vs Time plot with linear regression
#' plotResVsTime(
#' data = resVsTimeData, 
#' dataMapping = ResVsTimeDataMapping$new(x = "x", y = "y"),
#' smoother = "lm"
#' )
plotResVsTime <- plotResVsPred