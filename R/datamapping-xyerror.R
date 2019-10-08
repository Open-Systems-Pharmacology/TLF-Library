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
                              errorMax = NULL, ...) {
      super$initialize(x, y, ...)
      self$error <- error
      self$errorMin <- error %||% errorMin
      self$errorMax <- error %||% errorMax
    },

    getMapData = function(data, metaData) {
      x <- data[, self$x]
      y <- data[, self$y]
      errorMin <- data[, self$errorMin]
      errorMax <- data[, self$errorMax]

      color <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$color
      )

      shape <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$shape
      )

      size <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$size
      )

      return(list(
        "x" = x, "y" = y, "ymin" = y - errorMin, "ymax" = y + errorMax,
        "color" = color, "shape" = shape, "size" = size
      ))
    }
  )
)
