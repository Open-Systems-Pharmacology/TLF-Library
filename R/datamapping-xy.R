#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field data Dataset after mapping
#' @section Methods:
#' \describe{
#' \item{new(x, y=NULL)}{Initialize XYDataMapping.}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' }
#' @export
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    data = NULL,

    initialize = function(x, y = NULL) {
      if (isOfType(x, "numeric")) {
        warning(paste("'x' is a numeric, dataMapping will select the column of indice", x, "in data"))
      }
      if (isOfType(y, "numeric")) {
        warning(paste("'y' is a numeric, dataMapping will select the column of indice", y, "in data"))
      }
      self$x <- x
      self$y <- y
    },

    checkMapData = function(data, metaData = NULL) {
      if (isOfType(self$x, "character")) {
        validateMapping(self$x, data)
      }
      if (isOfType(self$y, "character")) {
        validateMapping(self$y, data)
      }

      # Drop option simplify data.frame into vectors
      # False enforces data to stay as data.frame if x or y is empty
      self$data <- data[, c(self$x, self$y), drop = FALSE]

      # Dummy variable for default aesthetics
      self$data$defaultAes <- factor("")
      return(self$data)
    }
  )
)
