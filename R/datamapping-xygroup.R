#' @title XYGDataMapping
#' @description  R6 class for mapping \code{x}, \code{y} and \code{GroupMapping} variables to \code{data}
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    #' @field groupMapping R6 class \code{GroupMapping} object
    groupMapping = NULL,

    #' @description Create a new \code{XYGDataMapping} object
    #' @param x Name of x variable to map
    #' @param y Name of y variable to map
    #' @param groupMapping R6 class \code{GroupMapping} object
    #' @param color R6 class \code{Grouping} object or its input
    #' @param fill R6 class \code{Grouping} object or its input
    #' @param linetype R6 class \code{Grouping} object or its input
    #' @param shape R6 class \code{Grouping} object or its input
    #' @param size R6 class \code{Grouping} object or its input
    #' @param data data.frame to map used by \code{smartMapping}
    #' @return A new \code{XYGDataMapping} object
    initialize = function(x = NULL,
                              y = NULL,
                              groupMapping = NULL,
                              color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL,
                              data = NULL) {

      # smartMapping is available in utilities-mapping.R
      smartMap <- smartMapping(data)
      super$initialize(x = x %||% smartMap$x, y = y %||% smartMap$y)

      validateEitherOrNullInput(groupMapping, list(
        "color" = color,
        "fill" = fill,
        "linetype" = linetype,
        "shape" = shape,
        "size" = size
      ))
      # To simplify the process workflow, groupMapping inputs color, fill... can be used directly instead of groupMapping
      self$groupMapping <- groupMapping %||% GroupMapping$new(
        color %||% smartMap$color,
        fill %||% smartMap$fill,
        linetype %||% smartMap$linetype,
        shape %||% smartMap$shape,
        size %||% smartMap$size
      )
    },

    #' @description Check that \code{data} variables include map variables
    #' @param data data.frame to check
    #' @param metaData list containing information on \code{data}
    #' @return A data.frame with map and \code{defaultAes} variables.
    #' Dummy variable \code{defaultAes} is necessary to allow further modification of plots.
    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data, nullAllowed = TRUE)
      validateMapping(self$y, data, nullAllowed = TRUE)

      # Drop option simplify data.frame into vectors
      # False enforces data to stay as data.frame if x or y is empty
      self$data <- data[, c(self$x, self$y), drop = FALSE]

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (isOfLength(self$groupMapping[[groupType]]$group, 0)) {next}
          grouping <- self$groupMapping[[groupType]]

          groupVariables <- grouping$group
          if (isOfType(groupVariables, "data.frame")) {
            # Last group variable is the label in group data.frames
            # and need to be removed from the check
            groupVariables <- names(groupVariables)
            groupVariables <- utils::head(groupVariables, -1)
          }
          validateMapping(groupVariables, data)
          # Enforce grouping variables to be factors
          self$data[, grouping$label] <- as.factor(grouping$getCaptions(data, metaData))
          
          # Dummy variable for default aesthetics that will be used to define legend labels
          legendLabels <- self$data$legendLabels %||% grouping$getCaptions(data, metaData)
          
          # Prevent duplication of legend if groupings are the same
          if(isTRUE(all.equal(legendLabels, grouping$getCaptions(data, metaData)))){
            self$data$legendLabels <- legendLabels
            next
          }
          self$data$legendLabels <- as.factor(paste(as.character(self$data$legendLabels), 
                                                    as.character(grouping$getCaptions(data, metaData)), 
                                                    sep = "-"))
      }

      if (is.null(self$data$legendLabels)) {
        self$data$legendLabels <- factor("")
      }
      return(self$data)
    }
  )
)
