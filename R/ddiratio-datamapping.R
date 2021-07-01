#' @title DDIRatioDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and \code{ddiRatioLines} variables to \code{data}
#' @export
DDIRatioDataMapping <- R6::R6Class(
  "DDIRatioDataMapping",
  inherit = PKRatioDataMapping,

  public = list(
    #' @field minRange Mininmum range for guest and ratio lines
    minRange = NULL,
    #' @field deltaGuest Value of `delta` in Guest et al. equation
    deltaGuest = NULL,

    #' @description Create a new \code{DDIRatioDataMapping} object
    #' @param deltaGuest Value of parameter `delta` in Guest et al. equation.
    #' Default value is 1.
    #' @param minRange Mininmum range for guest lines
    #' @param lines list of ratio limits to plot as diagonal lines
    #' @param ... parameters inherited from \code{PKRatioDataMapping}
    #' @return A new \code{DDIRatioDataMapping} object
    initialize = function(deltaGuest = 1,
                          minRange = c(1e-2, 1e2),
                          lines = DefaultDataMappingValues$ddiRatio,
                          ...) {
      super$initialize(...)
      # Apply log10 transformation to lines because 
      # plot is log scaled in by default and geom_abline 
      # requires the log transformed values in input of intercept
      self$lines <- lapply(lines, log10)
      self$minRange <- minRange
      self$deltaGuest <- deltaGuest
    }
  )
)
