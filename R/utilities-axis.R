#' @title setXAxis
#' @description Set X-axis properties on a ggplot object
#' @param plotObject ggplot object to set X-axis properties
#' @param scale Scale of X-axis. Use enum `Scaling` to access names of scales.
#' @param limits X-axis limits
#' @param ticks X-axis ticks
#' @param ticklabels X-axis ticklabels
#' @param font `Font` object  defining font of ticklabels
#' @return ggplot object with updated X-axis
#' @export
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

  xAxis$limits <- limits %||% xAxis$limits
  xAxis$scale <- scale %||% xAxis$scale
  xAxis$ticks <- ticks %||% xAxis$ticks
  xAxis$ticklabels <- ticklabels %||% xAxis$ticklabels
  xAxis$font <- font %||% xAxis$font

  newPlotObject <- xAxis$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setYAxis
#' @description Set Y-axis properties on a ggplot object
#' @param plotObject ggplot object to set Y-axis properties
#' @param scale Scale of Y-axis. Use enum `Scaling` to access names of scales.
#' @param limits Y-axis limits
#' @param ticks Y-axis ticks
#' @param ticklabels Y-axis ticklabels
#' @param font `Font` object  defining font of ticklabels
#' @return ggplot object with updated Y-axis
#' @export
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

  yAxis$limits <- limits %||% yAxis$limits
  yAxis$scale <- scale %||% yAxis$scale
  yAxis$ticks <- ticks %||% yAxis$ticks
  yAxis$ticklabels <- ticklabels %||% yAxis$ticklabels
  yAxis$font <- font %||% yAxis$font

  newPlotObject <- yAxis$updatePlot(newPlotObject)
  return(newPlotObject)
}

xAxisDefaultScale <- function(plotConfiguration) {
  if (isOfType(plotConfiguration, c("DDIRatioPlotConfiguration"))) {
    return(Scaling$log10)
  }
  if (isOfType(plotConfiguration, c("BoxWhiskerPlotConfiguration"))) {
    return(Scaling$discrete)
  }
  return(Scaling$lin)
}

yAxisDefaultScale <- function(plotConfiguration) {
  if (isOfType(plotConfiguration, c("PKRatioPlotConfiguration", "DDIRatioPlotConfiguration"))) {
    return(Scaling$log10)
  }
  return(Scaling$lin)
}
