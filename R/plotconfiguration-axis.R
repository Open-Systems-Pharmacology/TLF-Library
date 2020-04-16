#' @title Scaling
#' @include enum.R
#' @export
#' @description
#'  Pre-defined transformation of axes
#'  Not that built-in transformations from ggplot2 includes more transformations
Scaling <- enum(c(
  "lin",
  "log10",
  "log",
  "discrete",
  "reverse", "sqrt", "time", "date"
))


#' @title AxisConfiguration
#' @description  R6 class defining the configuration of axis
#' @export
AxisConfiguration <- R6::R6Class(
  "AxisConfiguration",
  public = list(
    #' @field limits numeric vector of axis limits
    limits = NULL,
    #' @field scale character defining axis scale
    scale = NULL,
    #' @field ticks numeric vector or function defining where to position axis ticks
    ticks = NULL,
    #' @field ticklabels character vector or function defining what to print on axis ticks
    ticklabels = NULL,

    #' @description Create a new \code{AxisConfiguration} object
    #' @param limits numeric vector of axis limits
    #' @param scale character defining axis scale
    #' Use enum `Scaling` to access predefined scales.
    #' @param ticks numeric vector or function defining where to position axis ticks
    #' @param ticklabels character vector or function defining what to print on axis ticks
    #' @return A new \code{AxisConfiguration} object
    initialize = function(limits = NULL,
                              scale = Scaling$lin,
                              ticks = "default",
                              ticklabels = "default") {
      self$limits <- limits
      self$scale <- scale
      if (self$scale %in% "lin") {
        self$scale <- "identity"
      }

      self$ticks <- ticks
      if (ticks %in% "default") {
        self$ticks <- waiver()
      }
      self$ticklabels <- ticklabels
      if (ticklabels %in% "default") {
        self$ticklabels <- waiver()
      }
    },

    #' @description Print axis properties
    #' @return Axis properties
    print = function() {
      axisProperties <- list(
        scale = self$scale,
        limits = self$limits,
        ticks = self$ticks,
        ticklabels = self$ticklabels
      )
      if (axisProperties$scale %in% "identity") {
        axisProperties$scale <- "linear"
      }
      if (length(axisProperties$ticks) == 0 & !is.null(axisProperties$ticks)) {
        axisProperties$ticks <- "default"
      }
      if (length(axisProperties$ticklabels) == 0 & !is.null(axisProperties$ticklabels)) {
        axisProperties$ticklabels <- "default"
      }
      return(axisProperties)
    }
  )
)

#' @title XAxisConfiguration
#' @description  R6 class defining the configuration of X-axis
#' @export
XAxisConfiguration <- R6::R6Class(
  "XAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    #' @description Set axis configuration on a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object with updated axis properties
    setPlotAxis = function(plotObject) {
      if (self$scale %in% "discrete") {
        plotObject <- plotObject +
          scale_x_discrete(limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
        return(plotObject)
      }
      plotObject <- plotObject +
        scale_x_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
      return(plotObject)
    }
  )
)

#' @title YAxisConfiguration
#' @description  R6 class defining the configuration of Y-axis
#' @export
YAxisConfiguration <- R6::R6Class(
  "YAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    #' @field position character poistion of the Y-axis
    position = NULL, # TO DO: initialize position, then scale position = "left" or "right"

    #' @description Set axis configuration on a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object with updated axis properties
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_y_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
      return(plotObject)
    }
  )
)

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

  # Clone tlfConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$tlfConfiguration <- plotObject$tlfConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$tlfConfiguration$yAxis
  xAxis <- newPlotObject$tlfConfiguration$xAxis

  xAxis <- plotObject$tlfConfiguration$xAxis$clone()
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

  # Clone tlfConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$tlfConfiguration <- plotObject$tlfConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$tlfConfiguration$yAxis
  yAxis <- newPlotObject$tlfConfiguration$yAxis

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
