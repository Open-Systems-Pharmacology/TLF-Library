#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
#'

XDataMapping <- R6::R6Class(
  "XDataMapping",
  public = list(
    x = NULL,
    data = NULL,

    initialize = function(x) {
      self$x <- x
    },

    getMapData = function(data, metaData = NULL) {


      # Takes dataframe as input.
      # Extracts x and y from dataframe.
      # Sets the object's data property to be a dataframe with an x column , y column, color column, shape column...
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- data.frame("x" = x, "y" = y)

      return(self$data)
    }
  )
)
