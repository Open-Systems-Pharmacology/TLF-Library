#' @title DDIRatioDataMapping
#' @docType class
#' @description  Data Mapping for DDI Ratio plots
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field groupMapping R6 class mapping groups to aesthetic properties
#' @field ddiRatioLines Values of the horizontal or diagonal lines plotted in DDI ratio plots
#' @field deltaGuest Value of delta for plotting DDI limits from Guest et al.
#' @field xmin Min value for DDI ratio lines and Guest et al. limits
#' @field xmax Max value for DDI ratio lines and Guest et al. limits
#' @section Methods:
#' \describe{
#' \item{new(ddiRatioLines = c(1, 2, 1 / 2), deltaGuest = 1, xmin = 1e-1, xmax = 1e1,
#' x, y, groupMapping = NULL, color = NULL, fill = NULL, linetype = NULL, shape = NULL, size = NULL)}{
#' Initialize DDIRatioDataMapping Either input groupMapping or input color, fill, linetype, shape and/or size.}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' \item{getDDIRatioLines()}{Get DDI ratio limits as a data.frame whose variables are x, y, ymax, ymin.}
#' \item{getGuestLines()}{Get Guest et al. limits as a data.frame whose variables are x, y, ymax, ymin.}
#' }
#' @export
DDIRatioDataMapping <- R6::R6Class(
  "DDIRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    ddiRatioLines = NULL,
    deltaGuest = NULL,
    xmin = NULL,
    xmax = NULL,

    initialize = function(ddiRatioLines = c(1, 2, 1 / 2),
                              deltaGuest = 1,
                              xmin = 1e-1,
                              xmax = 1e1,
                              x = "Observed",
                              y = "Simulated", ...) {
      super$initialize(x = x, y = y, ...)
      self$ddiRatioLines <- ddiRatioLines
      self$deltaGuest <- deltaGuest
      self$xmin <- xmin
      self$xmax <- xmax
    },

    # This function is needed because,
    # geom_abline doesn't work properly when plot scale is log
    getDDIRatioLines = function() {
      xmin <- self$xmin
      xmax <- self$xmax

      x <- 10^(seq(log10(xmin), log10(xmax), 0.01))

      y <- x * self$ddiRatioLines[1]
      ymax <- x * self$ddiRatioLines[2]
      ymin <- x * self$ddiRatioLines[3]

      ddiRatioLines <- data.frame(x, y, ymin, ymax)
      return(ddiRatioLines)
    },

    getGuestLines = function() {
      xmin <- self$xmin
      xmax <- self$xmax

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
