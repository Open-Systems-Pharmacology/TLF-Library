#' @title BoxWhiskerDataMapping
#' @docType class
#' @description  Class for Box Whisker Mapping
#' @export
BoxWhiskerDataMapping <- R6::R6Class(
  "BoxWhiskerDataMapping",
  inherit = XYGDataMapping,
  public = list(
    quantiles = NULL,
    outliers = NULL, 
    
    initialize = function(...,
                          quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                          outliers = NULL
                          ) {
      super$initialize(...)
      self$quantiles <- quantiles
      self$outliers <- outliers
    }
  )
)
