#' @title PKRatioDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, \code{GroupMapping} and pkRatio \code{lines} variables to \code{data}
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field lines list of ratio limits to plot as horizontal lines
    lines = NULL,
    #' @field uncertainty mapping error bars around scatter points
    uncertainty = NULL,

    #' @description Create a new \code{PKRatioDataMapping} object
    #' @param lines list of ratio limits to plot as horizontal lines
    #' @param uncertainty mapping error bars around scatter points
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{PKRatioDataMapping} object
    initialize = function(lines = DefaultDataMappingValues$pkRatio,
                              uncertainty = NULL,
                              ...) {
      validateIsString(uncertainty, nullAllowed = TRUE)
      super$initialize(...)
      self$lines <- lines
      self$uncertainty <- uncertainty
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      validateMapping(self$uncertainty, data, nullAllowed = TRUE)
      mapData <- super$checkMapData(data, metaData)
      # This may change depending of how we want to include options
      if (!isOfLength(self$uncertainty, 0)) {
        mapData$ymax <- data[, self$y] + data[, self$uncertainty]
        mapData$ymin <- data[, self$y] - data[, self$uncertainty]
      }
      self$data <- mapData
      return(mapData)
    }
  )
)
