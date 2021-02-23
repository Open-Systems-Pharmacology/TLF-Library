#' @title ObservedDataMapping
#' @description  R6 class for mapping \code{x}, \code{y}, of observed data for a time profile plot
#' @export
ObservedDataMapping <- R6::R6Class(
  "ObservedDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field lloq mapping lower limit of quantitation variable
    lloq = NULL,
    #' @field uncertainty mapping error bars around scatter points
    uncertainty = NULL,
    #' @field mdv mapping missing dependent variable
    mdv = NULL,
    #' @field ymin mapping error bars around scatter points
    ymin = "ymin",
    #' @field ymax mapping error bars around scatter points
    ymax = "ymax",

    #' @description Create a new \code{PKRatioDataMapping} object
    #' @param lloq mapping lower limit of quantitation variable
    #' @param uncertainty mapping error bars around scatter points
    #' @param mdv mapping missing dependent variable
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{PKRatioDataMapping} object
    initialize = function(lloq = NULL,
                              uncertainty = NULL,
                              mdv = NULL,
                              ...) {
      validateIsString(lloq, nullAllowed = TRUE)
      validateIsString(uncertainty, nullAllowed = TRUE)
      validateIsString(mdv, nullAllowed = TRUE)
      super$initialize(...)
      self$lloq <- lloq
      self$uncertainty <- uncertainty
      self$mdv <- mdv
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateIsOfType(data, "data.frame")
      validateIsIncluded(self$uncertainty, names(data), nullAllowed = TRUE)
      validateIsIncluded(self$lloq, names(data), nullAllowed = TRUE)
      validateIsIncluded(self$mdv, names(data), nullAllowed = TRUE)

      mapData <- super$checkMapData(data, metaData)
      # Add lloq data
      if (!isOfLength(self$lloq, 0)) {
        mapData[, self$lloq] <- data[, self$lloq]
        mapData$lloq <- data[, self$lloq]
      }
      # ymin and ymax for uncertainty error bars
      # This may change depending of how we want to include options
      if (!isOfLength(self$uncertainty, 0)) {
        mapData[, self$uncertainty] <- data[, self$uncertainty]
        mapData$ymax <- data[, self$y] + data[, self$uncertainty]
        mapData$ymin <- data[, self$y] - data[, self$uncertainty]
      }
      # MDV is a Nonmem notation in which values with MDV==1 are removed
      if (!isOfLength(self$mdv, 0)) {
        mapData <- mapData[!as.logical(data[, self$mdv]), ]
      }
      return(mapData)
    }
  )
)
