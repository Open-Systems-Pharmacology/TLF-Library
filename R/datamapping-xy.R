#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
#'
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    data = NULL,

    initialize = function(x, y = NULL) {
      self$x <- x
      self$y <- y
    },

    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data)
      validateMapping(self$y, data)

      self$data <- data[, c(self$x, self$y)]

      # Dummy variable for default aesthetics
      self$data$defaultAes <- factor("")
      return(self$data)
    }
  )
)
