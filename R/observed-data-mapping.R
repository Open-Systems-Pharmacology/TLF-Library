#' @title ObservedDataMapping
#' @description  R6 class for mapping `x`, `y`, of observed data for a time profile plot
#' @export
#' @family DataMapping classes
ObservedDataMapping <- R6::R6Class(
  "ObservedDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field error mapping error bars around scatter points
    error = NULL,
    #' @field mdv mapping missing dependent variable
    mdv = NULL,
    #' @field ymin mapping error bars around scatter points
    ymin = NULL,
    #' @field ymax mapping error bars around scatter points
    ymax = NULL,

    #' @description Create a new `ObservedDataMapping` object
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @param group R6 class `Grouping` object or its input
    #' @param color R6 class `Grouping` object or its input
    #' @param shape R6 class `Grouping` object or its input
    #' @param data data.frame to map used by `smartMapping`
    #' @param uncertainty mapping error bars around scatter points.
    #' Deprecated parameter replaced by `error`.
    #' @param error mapping error bars around scatter points
    #' @param ymin mapping lower end of error bars around scatter points
    #' @param ymax mapping upper end of error bars around scatter points
    #' @param mdv mapping missing dependent variable
    #' @return A new `ObservedDataMapping` object
    initialize = function(x,
                          y,
                          uncertainty = NULL,
                          error = NULL,
                          ymin = NULL,
                          ymax = NULL,
                          mdv = NULL,
                          color = NULL,
                          shape = NULL,
                          group = NULL,
                          data = NULL) {
      validateIsString(uncertainty, nullAllowed = TRUE)
      validateIsString(error, nullAllowed = TRUE)
      validateIsString(ymin, nullAllowed = TRUE)
      validateIsString(ymax, nullAllowed = TRUE)
      validateIsString(mdv, nullAllowed = TRUE)
      super$initialize(x = x, y = y, color = color, shape = shape, group = group, data = data)

      # If defined, ymin and ymax are used as is
      # If not, error/uncertainty are used and
      # creates ymin and ymax as y +/- error
      self$error <- error %||% uncertainty
      self$ymin <- ymin %||% ifNotNull(self$error, "ymin")
      self$ymax <- ymax %||% ifNotNull(self$error, "ymax")
      self$mdv <- mdv
    },

    #' @description Check that `data` variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on `data`
    #' @return A data.frame with map and `defaultAes` variables.
    #' Dummy variable `defaultAes` is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      validateIsIncluded(self$error, names(data), nullAllowed = TRUE)
      validateIsIncluded(self$mdv, names(data), nullAllowed = TRUE)

      # Using super method, fetches x, y and groups
      mapData <- super$checkMapData(data, metaData)

      # ymin and ymax for error bars
      # This section may change depending on how we want to include options
      if (!isEmpty(self$error)) {
        mapData[, self$error] <- data[, self$error]
        # Symetric error bars
        mapData[, self$ymax] <- data[, self$y] + data[, self$error]
        mapData[, self$ymin] <- data[, self$y] - data[, self$error]
      }
      # Overwrite ymin and ymax if user defined
      if (isIncluded(self$ymax, names(data))) {
        mapData[, self$ymax] <- data[, self$ymax]
      }
      if (isIncluded(self$ymin, names(data))) {
        mapData[, self$ymin] <- data[, self$ymin]
      }
      # MDV is a Nonmem notation in which values with MDV==1 are removed
      if (!isEmpty(self$mdv)) {
        mapData[, self$mdv] <- as.logical(data[, self$mdv])
        mapData <- mapData[!mapData[, self$mdv], ]
      }
      return(mapData)
    }
  )
)
