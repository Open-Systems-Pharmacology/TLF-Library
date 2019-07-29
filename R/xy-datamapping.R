#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
XYDataMapping <- R6::R6Class("XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    initialize = function(x, y) {
      self$x <- x
      self$y <- y
    }
  )
)
