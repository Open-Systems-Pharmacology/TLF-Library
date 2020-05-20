#' @title setXAxis
#' @description Set X-axis properties on a ggplot object
#' @param plotObject ggplot object to set X-axis properties
#' @param scale Scale of X-axis. Use enum `Scaling` to access names of scales.
#' @param limits X-axis limits
#' @param ticks X-axis ticks
#' @param ticklabels X-axis ticklabels
#' @return ggplot object with updated X-axis
#' @export
setXAxis <- function(plotObject,
                     scale = NULL,
                     limits = NULL,
                     ticks = NULL,
                     ticklabels = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(scale, Scaling, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  xAxis <- newPlotObject$plotConfiguration$xAxis

  xAxis <- plotObject$plotConfiguration$xAxis$clone()
  xAxis$limits <- limits %||% xAxis$limits
  xAxis$scale <- scale %||% xAxis$scale
  xAxis$ticks <- ticks %||% xAxis$ticks
  xAxis$ticklabels <- ticklabels %||% xAxis$ticklabels

  if (xAxis$scale %in% "lin") {
    xAxis$scale <- "identity"
  }
  if (length(xAxis$ticks) > 0) {
    if (xAxis$ticks %in% "default") {
      xAxis$ticks <- waiver()
    }
  }

  if (length(xAxis$ticklabels) > 0) {
    if (xAxis$ticklabels %in% "default") {
      xAxis$ticklabels <- waiver()
    }
  }

  newPlotObject <- xAxis$setPlotAxis(newPlotObject)

  return(newPlotObject)
}

#' @title setYAxis
#' @description Set Y-axis properties on a ggplot object
#' @param plotObject ggplot object to set Y-axis properties
#' @param scale Scale of Y-axis. Use enum `Scaling` to access names of scales.
#' @param limits Y-axis limits
#' @param ticks Y-axis ticks
#' @param ticklabels Y-axis ticklabels
#' @return ggplot object with updated Y-axis
#' @export
setYAxis <- function(plotObject,
                     scale = NULL,
                     limits = NULL,
                     ticks = NULL,
                     ticklabels = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(scale, Scaling, nullAllowed = TRUE)

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

  if (yAxis$scale %in% "lin") {
    yAxis$scale <- "identity"
  }

  if (length(yAxis$ticks) > 0) {
    if (yAxis$ticks %in% "default") {
      yAxis$ticks <- waiver()
    }
  }

  if (length(yAxis$ticklabels) > 0) {
    if (yAxis$ticklabels %in% "default") {
      yAxis$ticklabels <- waiver()
    }
  }

  newPlotObject <- yAxis$setPlotAxis(newPlotObject)

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
