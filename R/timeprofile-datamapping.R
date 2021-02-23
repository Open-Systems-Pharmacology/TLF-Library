#' @title TimeProfileDataMapping
#' @description  R6 class defining the configuration of a \code{ggplot} object for time profile plot
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = RangeDataMapping,
  public = list(
    #' @description Create a new \code{TimeProfileDataMapping} object
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @param ymin Name of ymin variable to map
    #' @param ymax Name of ymax variable to map
    #' @param groupMapping R6 class \code{GroupMapping} object
    #' @param color R6 class \code{Grouping} object or its input
    #' @param fill R6 class \code{Grouping} object or its input
    #' @param linetype R6 class \code{Grouping} object or its input
    #' @param shape R6 class \code{Grouping} object or its input
    #' @param size R6 class \code{Grouping} object or its input
    #' @param data data.frame to map used by \code{smartMapping}
    #' @return A new \code{RangeDataMapping} object
    initialize = function(x = NULL,
                              y = NULL,
                              ymin = NULL,
                              ymax = NULL,
                              groupMapping = NULL,
                              color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL,
                              data = NULL) {

      # smartMapping is available in utilities-mapping.R
      smartMap <- smartMapping(data)
      super$initialize(
        x = x %||% smartMap$x,
        ymin = ymin %||% smartMap$ymin,
        ymax = ymax %||% smartMap$ymax,
        groupMapping = groupMapping, color = color, fill = fill,
        linetype = linetype, shape = shape, size = size
      )
      # Since TimeProfileDataMapping inherits from RangeDataMapping
      # super$initialize introduce a self$y which is NULL
      self$y <- y %||% smartMap$y
    },
    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{legendLabels} variables.
    #' Dummy variable \code{legendLabels} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      validateMapping(self$y, data, nullAllowed = TRUE)
      mapData <- super$checkMapData(data, metaData)
      # This may change depending of how we want to include options
      if (!isOfLength(self$y, 0)) {
        mapData[, self$y] <- data[, self$y]
      }
      self$data <- mapData
      return(mapData)
    }
  )
)
