#' @title PKRatioDataMapping
#' @description  R6 class for mapping `x`, `y`, `GroupMapping` and pkRatio `lines` variables to `data`
#' @export
#' @family DataMapping classes
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field lines list of ratio limits to plot as horizontal lines
    lines = NULL,
    #' @field error mapping error bars around scatter points
    error = NULL,

    #' @description Create a new `PKRatioDataMapping` object
    #' @param lines List of ratio limits to display as horizontal lines
    #' @param uncertainty mapping error bars around scatter points.
    #' Deprecated parameter replaced by `error`.
    #' @param error mapping error bars around scatter points
    #' @param ... parameters inherited from `XYGDataMapping`
    #' @return A new `PKRatioDataMapping` object
    initialize = function(lines = DefaultDataMappingValues$pkRatio,
                          uncertainty = NULL,
                          error = NULL,
                          ...) {
      validateIsString(uncertainty, nullAllowed = TRUE)
      super$initialize(...)
      self$lines <- lines
      # Keep uncertainty for compatibility
      self$error <- error %||% uncertainty
    },

    #' @description Check that `data` variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on `data`
    #' @return A data.frame with map and `defaultAes` variables.
    #' Dummy variable `defaultAes` is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      .validateMapping(self$error, data, nullAllowed = TRUE)
      mapData <- super$checkMapData(data, metaData)
      # This may change depending of how we want to include options
      if (!isOfLength(self$error, 0)) {
        mapData$ymax <- data[, self$y] * (1 + data[, self$error])
        mapData$ymin <- data[, self$y] * (1 - data[, self$error])
      }
      self$data <- mapData
      return(mapData)
    }
  )
)
