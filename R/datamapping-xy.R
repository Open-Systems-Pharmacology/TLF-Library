#' @title XYDataMapping
#' @description  R6 class for mapping \code{x} and \code{y} variable to \code{data}
#' @export
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    #' @field x Name of x variable to map
    x = NULL,
    #' @field y Name of y variable to map
    y = NULL,
    #' @field data data.frame used for mapping
    data = NULL,

    #' @description Create a new \code{XYDataMapping} object
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @return A new \code{XYDataMapping} object
    initialize = function(x, 
                          y = NULL) {
      
      if (isOfType(x, "numeric")) {
        warning(paste("'x' is a numeric, dataMapping will select the column of indice", x, "in data"))
      }
      if (isOfType(y, "numeric")) {
        warning(paste("'y' is a numeric, dataMapping will select the column of indice", y, "in data"))
      }
      
      self$x <- x
      self$y <- y
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
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

#' @title RangeDataMapping
#' @description  R6 class for mapping \code{x}, \code{ymin} and \code{ymax} variable to \code{data}
#' @export
RangeDataMapping <- R6::R6Class(
  "RangeDataMapping",
  inherit = XYDataMapping,
  public = list(
    #' @field ymin Name of ymin variable to map
    ymin = NULL,
    #' @field ymax Name of ymax variable to map
    ymax = NULL,
    
    #' @description Create a new \code{RangeDataMapping} object
    #' @param x Name of x variable to map
    #' @param ymin Name of ymin variable to map
    #' @param ymax Name of ymax variable to map
    #' @param data data.frame to map used by \code{smartMapping} 
    #' @return A new \code{RangeDataMapping} object    
    initialize = function(x = NULL,
                              ymin = NULL,
                              ymax = NULL,
                              data = NULL) {

      # smartMapping is available in utilities-mapping.R
      smartMap <- smartMapping(data)
      super$initialize(x %||% smartMap$x, y = NULL)

      self$ymin <- ymin %||% smartMap$ymin
      self$ymax <- ymax %||% smartMap$ymax
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      if (isOfType(self$x, "character")) {
        validateMapping(self$x, data)
      }
      if (isOfType(self$ymin, "character")) {
        validateMapping(self$ymin, data)
      }
      if (isOfType(self$ymax, "character")) {
        validateMapping(self$ymax, data)
      }

      # Drop option simplify data.frame into vectors
      # False enforces data to stay as data.frame if x or y is empty
      self$data <- data[, c(self$x, self$ymin, self$ymax), drop = FALSE]

      # Dummy variable for default aesthetics
      self$data$defaultAes <- factor("")
      return(self$data)
    }
  )
)
