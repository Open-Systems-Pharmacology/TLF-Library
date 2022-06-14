#' @title TimeProfileDataMapping
#' @description  R6 class defining the configuration of a `ggplot` object for time profile plot
#' @export
#' @family DataMapping classes
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = RangeDataMapping,
  public = list(
    #' @description Create a new `TimeProfileDataMapping` object
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @param ymin Name of ymin variable to map
    #' @param ymax Name of ymax variable to map
    #' @param group R6 class `Grouping` object or its input
    #' @param color R6 class `Grouping` object or its input
    #' @param fill R6 class `Grouping` object or its input
    #' @param linetype R6 class `Grouping` object or its input
    #' @param data data.frame to map used by `.smartMapping`
    #' @return A new `RangeDataMapping` object
    initialize = function(x = NULL,
                          y = NULL,
                          ymin = NULL,
                          ymax = NULL,
                          group = NULL,
                          color = NULL,
                          fill = NULL,
                          linetype = NULL,
                          data = NULL) {

      # .smartMapping is available in utilities-mapping.R
      smartMap <- .smartMapping(data)
      super$initialize(
        x = x %||% smartMap$x,
        ymin = ymin %||% smartMap$ymin,
        ymax = ymax %||% smartMap$ymax,
        color = color, fill = fill, linetype = linetype, group = group
      )
      # Since TimeProfileDataMapping inherits from RangeDataMapping
      # super$initialize introduce a self$y which is NULL
      self$y <- y %||% smartMap$y
    },
    #' @description Check that `data` variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on `data`
    #' @return A data.frame with map and `legendLabels` variables.
    #' Dummy variable `legendLabels` is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      .validateMapping(self$y, data, nullAllowed = TRUE)
      mapData <- super$checkMapData(data, metaData)
      # This may change depending of how we want to include options
      if (!isEmpty(self$y)) {
        mapData[, self$y] <- data[, self$y]
      }
      self$data <- mapData
      return(mapData)
    }
  )
)
