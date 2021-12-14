#' @title Scaling
#' @include enum.R
#' @export
#' @description
#'  Helper enum of predefined transformations of axes
#'  Note that the transformations will be translated internally into `ggplot2` transformations.
#'  `ggplot2` includes more transformations than what is available in this enum.
#' @family enum helpers
#' @examples
#' # Continuous linear scale
#' Scaling$lin
#' # Continuous log10 scale
#' Scaling$log
#' # Continuous natural logarithm (ln) scale (base is *e*)
#' Scaling$ln
#' # Discrete scale for categrical data such as boxplot and tornado plot data
#' Scaling$discrete
#' # Reverse continuous linear scale to switch end and beginning of linear scale
#' Scaling$reverse
#' # Continusous square root scale
#' Scaling$sqrt
#' # Time scale for POSIXlt or POSIXct data
#' Scaling$time
#' # Date scale for POSIXlt or POSIXct data
#' Scaling$date
#' 
Scaling <- enum(c(
  "lin",
  "log",
  "ln",
  "discrete",
  "reverse",
  "sqrt",
  "time",
  "date"
))

#' @title setXAxis
#' @description Set X-axis properties of a `ggplot` object
#' @param plotObject A `ggplot` object to set X-axis properties
#' @param scale Scale of axis. Use enum `Scaling` to access names of scales.
#' @param limits Optional numeric values of axis limits
#' @param ticks Optional values or function for axis ticks
#' @param ticklabels Optional values or function for axis ticklabels
#' @param font A `Font` object  defining font of ticklabels
#' @return A `ggplot` object
#' @export
#' @examples
#' myPlot <- addLine(x = c(1, 2, 3), y = c(10, 50, 100))
#'
#' # Set x-axis in log scale
#' setXAxis(myPlot, scale = Scaling$log)
#'
#' # Set x-axis ticklabels to Greek letters
#' setXAxis(myPlot, ticks = c(1, 2, 3), ticklabels = parse(text = c("alpha", "beta", "gamma")))
#'
#' # Set x-axis limits
#' setXAxis(myPlot, limits = c(1, 2.5))
#'
#' # Set x-axis fonts
#' setXAxis(myPlot, font = Font$new(color = "blue", size = 14))
setXAxis <- function(plotObject,
                     scale = NULL,
                     limits = NULL,
                     ticks = NULL,
                     ticklabels = NULL,
                     font = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(scale, Scaling, nullAllowed = TRUE)
  validateIsNumeric(limits, nullAllowed = TRUE)
  validateIsOfType(font, "Font", nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$xAxis
  xAxis <- newPlotObject$plotConfiguration$xAxis
  eval(parseVariableToObject("xAxis", c("limits", "scale", "ticks", "ticklabels", "font"), keepIfNull = TRUE))
  newPlotObject <- xAxis$updatePlot(newPlotObject, ylim = newPlotObject$plotConfiguration$yAxis$limits)
  return(newPlotObject)
}

#' @title setYAxis
#' @description Set Y-axis properties of a `ggplot` object
#' @inheritParams setXAxis
#' @return A `ggplot` object
#' @export
#' @examples
#' myPlot <- addLine(x = c(1, 2, 3), y = c(10, 50, 100))
#'
#' # Set y-axis in log scale
#' setYAxis(myPlot, scale = Scaling$log)
#'
#' # Set y-axis ticklabels to Greek letters
#' setYAxis(myPlot, ticks = c(10, 50, 100), ticklabels = parse(text = c("alpha", "beta", "gamma")))
#'
#' # Set y-axis limits
#' setYAxis(myPlot, limits = c(10, 75))
#'
#' # Set y-axis fonts
#' setYAxis(myPlot, font = Font$new(color = "blue", size = 14))
setYAxis <- function(plotObject,
                     scale = NULL,
                     limits = NULL,
                     ticks = NULL,
                     ticklabels = NULL,
                     font = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(scale, Scaling, nullAllowed = TRUE)
  validateIsNumeric(limits, nullAllowed = TRUE)
  validateIsOfType(font, "Font", nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  yAxis <- newPlotObject$plotConfiguration$yAxis
  eval(parseVariableToObject("yAxis", c("limits", "scale", "ticks", "ticklabels", "font"), keepIfNull = TRUE))
  newPlotObject <- yAxis$updatePlot(newPlotObject, xlim = newPlotObject$plotConfiguration$xAxis$limits)
  return(newPlotObject)
}

#' @title xAxisDefaultScale
#' @description Return x-axis default scale from a plot configuration
#' @param plotConfiguration A `PlotConfiguration` object
#' @return The default scale.
#' The enum `Scaling` provides a list of available scales.
#' @examples
#' \dontrun{
#' # Regular plots use continuous linear scale for x-axis
#' plotConfiguration <- PlotConfiguration$new()
#' xAxisDefaultScale(plotConfiguration)
#'
#' # DDI plots use log scale for x-axis
#' ddiPlotConfiguration <- DDIRatioPlotConfiguration$new()
#' xAxisDefaultScale(ddiPlotConfiguration)
#'
#' # Boxplots use discrete scale for x-axis
#' boxPlotConfiguration <- BoxWhiskerPlotConfiguration$new()
#' xAxisDefaultScale(boxPlotConfiguration)
#' }
#' @keywords internal
xAxisDefaultScale <- function(plotConfiguration) {
  if (isOfType(plotConfiguration, c("DDIRatioPlotConfiguration"))) {
    return(Scaling$log)
  }
  if (isOfType(plotConfiguration, c("BoxWhiskerPlotConfiguration"))) {
    return(Scaling$discrete)
  }
  return(Scaling$lin)
}

#' @title yAxisDefaultScale
#' @description Return y-axis default scale from a plot configuration
#' @param plotConfiguration A `PlotConfiguration` object
#' @return The default scale.
#' The enum `Scaling` provides a list of available scales.
#' @examples
#' \dontrun{
#' # Regular plots use continuous linear scale for x-axis
#' plotConfiguration <- PlotConfiguration$new()
#' yAxisDefaultScale(plotConfiguration)
#'
#' # DDI plots use log scale for y-axis
#' ddiPlotConfiguration <- DDIRatioPlotConfiguration$new()
#' yAxisDefaultScale(ddiPlotConfiguration)
#'
#' # Tornado plots use discrete scale for y-axis
#' tornadoPlotConfiguration <- TornadoPlotConfiguration$new()
#' yAxisDefaultScale(tornadoPlotConfiguration)
#' }
#' @keywords internal
yAxisDefaultScale <- function(plotConfiguration) {
  if (isOfType(plotConfiguration, c("PKRatioPlotConfiguration", "DDIRatioPlotConfiguration"))) {
    return(Scaling$log)
  }
  if (isOfType(plotConfiguration, c("TornadoPlotConfiguration"))) {
    return(Scaling$discrete)
  }
  return(Scaling$lin)
}

#' @title getLogTickLabels
#' @description Get ticklabels expressions for log scale plots
#' @param ticks numeric values of the ticks
#' @return Expressions to use in `ticklabels` input parameter of `setXAxis` and `setYAxis` functions
#' @examples
#' ticks <- c(1, 5, 10, 50, 100, 500)
#' getLogTickLabels(ticks)
#' @export
getLogTickLabels <- function(ticks) {
  exponentValues <- floor(log10(ticks))
  # Values to print before 10^ using multiplication dot
  prefixValues <- ticks * 10^(-exponentValues)
  prefixValues <- paste0(prefixValues, "%.%")
  # For 1 the multiplication is redundant and removed
  prefixValues[prefixValues == "1%.%"] <- ""
  return(parse(text = paste(prefixValues, "10^", exponentValues, sep = "")))
}

#' @title getLnTickLabels
#' @description Get ticklabels expressions for ln scale plots
#' @param ticks numeric values of the ticks
#' @return Expressions to use in `ticklabels` input parameter of `setXAxis` and `setYAxis` functions
#' @examples
#' ticks <- exp(c(1, 5, 10, 50, 100, 500))
#' getLnTickLabels(ticks)
#' @export
getLnTickLabels <- function(ticks) {
  exponentValues <- floor(log(ticks))
  # Values to print before 10^ using multiplication dot
  prefixValues <- ticks * exp(-exponentValues)
  prefixValues <- paste0(prefixValues, "%.%")
  # For 1 the multiplication is redundant and removed
  prefixValues[prefixValues == "1%.%"] <- ""
  return(parse(text = paste(prefixValues, "e^", exponentValues, sep = "")))
}


#' @title getSqrtTickLabels
#' @description Get ticklabels expressions for sqrt scale plots
#' @param ticks numeric values of the ticks
#' @return Expressions to use in `ticklabels` input parameter of `setXAxis` and `setYAxis` functions
#' @examples
#' ticks <- sqrt(c(1, 5, 10, 50, 100, 500))
#' getSqrtTickLabels(ticks)
#' @export
getSqrtTickLabels <- function(ticks) {
  sqrtValues <- ticks^2
  return(parse(text = paste("sqrt(", sqrtValues, ")", sep = "")))
}
