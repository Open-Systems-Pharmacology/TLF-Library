#' @title DDIRatioDataMapping
#' @docType class
#' @description  Data Mapping for DDI Ratio plots
#' @export
DDIRatioDataMapping <- R6::R6Class(
  "DDIRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    ddiRatioLines = NULL,
    deltaGuest = NULL,
    # Example of how to do some other stuff
    initialize = function(ddiRatioLines = c(1, 2, 1 / 2),
                              deltaGuest = 1,
                              x = "Observed",
                              y = "Simulated", ...) {
      super$initialize(x = x, y = y, ...)
      self$ddiRatioLines <- ddiRatioLines
      self$deltaGuest <- deltaGuest
    },

    # This function is needed because,
    # geom_abline doesn't work properly when plot scale is log
    getDDIRatioLines = function(xmin = 1e-2, xmax = 1e2) {
      x <- 10^(seq(log10(xmin), log10(xmax), 0.01))

      y <- x * self$ddiRatioLines[1]
      ymax <- x * self$ddiRatioLines[2]
      ymin <- x * self$ddiRatioLines[3]

      ddiRatioLines <- data.frame(x, y, ymin, ymax)
      return(ddiRatioLines)
    },

    getGuestLines = function(xmin = 1e-2, xmax = 1e2) {
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
