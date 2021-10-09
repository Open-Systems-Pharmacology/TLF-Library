#' @title PKRatioDataMapping
#' @description  R6 class for mapping `x`, `y`, `GroupMapping` and pkRatio `lines` variables to `data`
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field lines list of ratio limits to plot as horizontal lines
    lines = NULL,
    #' @field uncertainty mapping error bars around scatter points
    uncertainty = NULL,

    #' @description Create a new `PKRatioDataMapping` object
    #' @param lines list of ratio limits to plot as horizontal lines
    #' @param uncertainty mapping error bars around scatter points
    #' @param ... parameters inherited from `XYGDataMapping`
    #' @return A new `PKRatioDataMapping` object
    initialize = function(lines = DefaultDataMappingValues$pkRatio,
                          uncertainty = NULL,
                          ...) {
      validateIsString(uncertainty, nullAllowed = TRUE)
      super$initialize(...)

      # Apply log10 transformation to lines because
      # plot is log scaled in by default and geom_abline
      # requires the log transformed values in input of intercept
      self$lines <- lapply(lines, log10)
      self$uncertainty <- uncertainty
    },

    #' @description Check that `data` variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on `data`
    #' @return A data.frame with map and `defaultAes` variables.
    #' Dummy variable `defaultAes` is necessary to allow further modification of plots.
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
