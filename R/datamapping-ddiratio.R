#' @title DDIRatioDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{ddiRatioLines} variables to \code{data}
#' @export
DDIRatioDataMapping <- R6::R6Class(
  "DDIRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field ddiRatioLines numeric vector of ratio limits to plot
    ddiRatioLines = NULL,
    #' @field deltaGuest numeric value of Guest et al ratio limits
    deltaGuest = NULL,
    #' @field range 2 elements vector of x limits
    range = NULL,


    #' @description Create a new \code{DDIRatioDataMapping} object
    #' @param ddiRatioValues list of values for ratio and guest limits to plot
    #' @param range 2 elements vector of x limits
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{DDIRatioDataMapping} object
    initialize = function(ddiRatioValues = DefaultDataMappingValues$ddiRatio,
                              range = c(1e-2, 1e2),
                              ...) {
      super$initialize(...)
      self$ddiRatioLines <- c(ddiRatioValues$ddiRatio1, ddiRatioValues$ddiRatio2)
      self$deltaGuest <- ddiRatioValues$guestLine
      self$range <- range
    },

    #' @description Create a data.frame with DDI ratio limits
    #' This data.frame is necessary in case if log-log plots as
    #' \code{geom_abline} doesn't work properly in log scale
    #' @return A data.frame with DDI ratio limits
    getDDIRatioLines = function() {
      xmin <- min(self$range)
      xmax <- max(self$range)

      x <- 10^(seq(log10(xmin), log10(xmax), 0.01))

      y <- x * self$ddiRatioLines[1]
      ymax <- x * self$ddiRatioLines[2]
      ymin <- x * self$ddiRatioLines[3]

      ddiRatioLines <- data.frame(x, y, ymin, ymax)
      return(ddiRatioLines)
    },

    #' @description Get a data.frame with Guest et al ratio limits
    #' @return A data.frame with Guest et al ratio limits
    getGuestLines = function() {
      xmin <- min(self$range)
      xmax <- max(self$range)

      x <- 10^(seq(log10(xmin), log10(xmax), 0.01))
      xSym <- x
      xSym[x < 1] <- 1 / x[x < 1]

      limit <- (self$deltaGuest + 2 * (xSym - 1)) / xSym

      ymin <- x / limit
      ymax <- x * limit

      guestLines <- data.frame(x, ymin, ymax)
      return(guestLines)
    }
  )
)
