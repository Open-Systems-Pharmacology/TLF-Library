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
        suppressMessages(
          plotObject <- plotObject +
            scale_x_discrete(limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
        )
        return(plotObject)
      }
      suppressMessages(
        plotObject <- plotObject +
          scale_x_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
      )
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
      suppressMessages(
        plotObject <- plotObject +
          scale_y_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
      )
      return(plotObject)
    }
  )
)
