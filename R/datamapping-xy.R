#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
#'

XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  inherit = XDataMapping,
  public = list(
    y = NULL,

    initialize = function(x, y) {
      super$initialize(x)
      self$y <- y
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
