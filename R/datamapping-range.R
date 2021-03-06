#' @title RangeDataMapping
#' @description  R6 class for mapping \code{x}, \code{ymin} and \code{ymax} variable to \code{data}
#' @export
RangeDataMapping <- R6::R6Class(
  "RangeDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field ymin Name of ymin variable to map
    ymin = NULL,
    #' @field ymax Name of ymax variable to map
    ymax = NULL,

    #' @description Create a new \code{RangeDataMapping} object
    #' @param x Name of x variable to map
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
      super$initialize(x %||% smartMap$x,
        y = NULL,
        groupMapping = groupMapping, color = color, fill = fill,
        linetype = linetype, shape = shape, size = size
      )

      self$ymin <- ymin %||% smartMap$ymin
      self$ymax <- ymax %||% smartMap$ymax
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{legendLabels} variables.
    #' Dummy variable \code{legendLabels} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data, nullAllowed = TRUE)
      if (isOfType(self$ymin, "character")) {
        validateMapping(self$ymin, data)
      }
      if (isOfType(self$ymax, "character")) {
        validateMapping(self$ymax, data)
      }

      # Drop option simplify data.frame into vectors
      # False enforces data to stay as data.frame if x or y is empty
      self$data <- data[, c(self$x, self$ymin, self$ymax), drop = FALSE]

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
          # Dummy variable for default aesthetics
          # Will be used to define legend labels
          self$data$legendLabels <- ifnotnull(
            self$data$legendLabels,
            paste(self$data$legendLabels, grouping$getCaptions(data, metaData), sep = "-"),
            grouping$getCaptions(data, metaData)
          )
        }
      }

      if (is.null(self$data$legendLabels)) {
        self$data$legendLabels <- factor("")
      }
      return(self$data)
    }
  )
)
