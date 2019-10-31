#' @title HistogramDataMapping
#' @docType class
#' @description  Class for Histogram Group Mapping
#' @export
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XYGDataMapping,
  public = list(
      verticalLineGroupings = NULL,

      initialize = function( x, y=NULL , groupings=NULL , verticalLineGroupings = NULL ) {
        super$initialize(x,y,groupings)
        self$verticalLineGroupings <- verticalLineGroupings
      }
  )
)
