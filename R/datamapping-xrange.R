#' @title XRangeDataMapping
#' @docType class
#' @description Abstract class for X Range Mapping
#' @export
XRangeDataMapping <- R6::R6Class(
  "XRangeDataMapping",
  inherit = XYDataMapping,
  public = list(
    yMin = NULL,
    yMax = NULL,

    # Example of how to do some other stuff
    initialize = function(x, yMin, yMax) {
      super$initialize(x, y = NULL, ...)
      self$yMin <- yMin
      self$yMax <- yMax
    },

    getMappedData = function(data, metaData) {
      x <- data[, self$x]
      yMin <- data[, self$yMin]
      yMin <- data[, self$yMin]

      color <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$color
      )

      return(list("x" = x, "yMin" = yMin, "yMax" = yMax, "color" = color))
    }
  )
)
