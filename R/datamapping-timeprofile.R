#' @title TimeProfileDataMapping
#' @description  R6 class defining the configuration of a \code{ggplot} object for time profile plot
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
    #' @param ... parameters inherited from \code{XYGDataMapping}
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
