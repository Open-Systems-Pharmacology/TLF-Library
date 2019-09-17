#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    initialize = function(x, y) {
      self$x <- x
      self$y <- y
    }
  )
)


#' @title XYEDataMapping
#' @docType class
#' @description Abstract class for X Y Error Mapping
#' @export
XYEDataMapping <- R6::R6Class(
  "XYEDataMapping",
  inherit = XYDataMapping,
  public = list(
    error = NULL,
    errorMin = NULL,
    errorMax = NULL,

    # Example of how to do some other stuff
    initialize = function(x, y,
                          error = NULL,
                          errorMin = NULL,
                          errorMax = NULL) {
      super$initialize(x, y)
      self$error <- error
      self$errorMin <- error %||% errorMin
      self$errorMax <- error %||% errorMax
    }
  )
)
