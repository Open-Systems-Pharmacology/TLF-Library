#' @title TimeProfileDataMapping
#' @docType class
#' @description  Data Mapping for Time Profile
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field ymin Name of ymin variable to map
#' @field ymax Name of ymax variable to map
#' @field groupMapping R6 class mapping groups to aesthetic properties
#' @field LLOQ Value(s) of lower limit of quantitation
#' @section Methods:
#' \describe{
#' \item{new(LLOQ = NULL, x, y, y = NULL, ymin = NULL, ymax = NULL, groupMapping = NULL, color = NULL, fill = NULL, linetype = NULL, shape = NULL, size = NULL)}{
#' Initialize TimeProfileDataMapping.  Either input groupMapping or input color, fill, linetype, shape and/or size.}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' }
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = XYGDataMapping,

  public = list(
    #' @field ymin Name of ymin variable to map
    ymin = NULL,
    #' @field ymax Name of ymax variable to map
    ymax = NULL,
    #' @field LLOQ values of lower limit of quatification
    LLOQ = NULL,

    #' @description Create a new \code{TimeProfileDataMapping} object
    #' @param LLOQ values of lower limit of quatification
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
    #' @return A new \code{TimeProfileDataMapping} object
    initialize = function(LLOQ = NULL,
                              x,
                              y = NULL,
                              ymin = NULL,
                              ymax = NULL, ...) {
      if (is.null(y)) {
        if (is.null(ymin) && is.null(ymax)) {
          stop("At least y or ymin and ymax must be defined for TimeProfileDataMapping")
        }
      }

      super$initialize(x = x, y = y, ...)

      self$ymin <- ymin
      self$ymax <- ymax
      self$LLOQ <- LLOQ
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data)
      validateMapping(self$y, data, nullAllowed = TRUE)
      validateMapping(self$ymin, data, nullAllowed = TRUE)
      validateMapping(self$ymax, data, nullAllowed = TRUE)

      self$data <- data[, c(self$x, self$y, self$ymin, self$ymax)]

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (!is.null(self$groupMapping[[groupType]]$group)) {
          grouping <- self$groupMapping[[groupType]]

          groupVariables <- grouping$group
          if (isOfType(groupVariables, "data.frame")) {
            # Last group variable is the label in group data.frames
            # and need to be removed from the check
            groupVariables <- names(groupVariables)
            groupVariables <- utils::head(groupVariables, -1)
          }
          validateMapping(groupVariables, data)

          self$data[, grouping$label] <- grouping$getCaptions(data, metaData)
        }
      }
      # Dummy variable for default aesthetics
      self$data$defaultAes <- factor("")
      return(self$data)
    }
  )
)
