#' @title AxisConfiguration
#' @docType class
#' @description  Generic axis Configuration
#' @export
AxisConfiguration <- R6::R6Class(
  "AxisConfiguration",
  public = list(
    limits = NULL,
    scale = NULL,
    ticks = NULL,
    ticklabels = NULL,
    
    initialize = function(limits = NULL,
                          scale = "lin",
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
#' @description  Generic X axis Configuration
#' @export
XAxisConfiguration <- R6::R6Class(
  "XAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_x_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
    }
  )
)

#' @title YAxisConfiguration
#' @docType class
#' @description  Generic Y axis Configuration
#' @export
YAxisConfiguration <- R6::R6Class(
  "YAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_y_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
    }
  )
)