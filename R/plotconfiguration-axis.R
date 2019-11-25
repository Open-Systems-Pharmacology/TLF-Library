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
#' @docType class
#' @description  Class for axis Configuration
#' @field limits Axis limits
#' @field scale Plot scale (lin, log...)
#' @field ticks Values where ticks should be applied
#' @field ticklabels Names of associated to ticks
#' @section Methods:
#' \describe{
#' \item{new(limits = NULL, scale = Scaling$lin, ticks = "default", ticklabels = "default")}{Initialize AxisConfiguration.}
#' }
#' @export
AxisConfiguration <- R6::R6Class(
  "AxisConfiguration",
  public = list(
    limits = NULL,
    scale = NULL,
    ticks = NULL,
    ticklabels = NULL,

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
    }
  )
)

#' @title XAxisConfiguration
#' @docType class
#' @description  Class for X-axis Configuration
#' @field limits Axis limits
#' @field scale Plot scale (lin, log...)
#' @field ticks Values where ticks should be applied
#' @field ticklabels Names of associated to ticks
#' @section Methods:
#' \describe{
#' \item{new(limits = NULL, scale = Scaling$lin, ticks = "default", ticklabels = "default")}{Initialize AxisConfiguration.}
#' \item{setPlotAxis(plotObject)}{Apply properties to plot X-axis.}
#' }
#' @export
XAxisConfiguration <- R6::R6Class(
  "XAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    setPlotAxis = function(plotObject) {
      if(self$scale %in% "discrete"){
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
#' @docType class
#' @description  Class for Y-axis Configuration
#' @field limits Axis limits
#' @field scale Plot scale (lin, log...)
#' @field ticks Values where ticks should be applied
#' @field ticklabels Names of associated to ticks
#' @section Methods:
#' \describe{
#' \item{new(limits = NULL, scale = Scaling$lin, ticks = "default", ticklabels = "default")}{Initialize AxisConfiguration.}
#' \item{setPlotAxis(plotObject)}{Apply properties to plot X-axis.}
#' }
#' @export
YAxisConfiguration <- R6::R6Class(
  "YAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    position = NULL, # TO DO: initialize position, then scale position = "left" or "right"
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_y_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
    }
  )
)
