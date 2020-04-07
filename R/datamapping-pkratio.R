#' @title PKRatioDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{pkRatioLines} variables to \code{data}
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field pkRatioLines numeric vector of ratio limits to plot
    pkRatioLines = NULL,

    #' @description Create a new \code{PKRatioDataMapping} object
    #' @param pkRatioLines numeric vector of ratio limits to plot
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{PKRatioDataMapping} object
    initialize = function(pkRatioLines = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
                              ...) {
      super$initialize(...)
      self$pkRatioLines <- pkRatioLines
    }
  )
)
