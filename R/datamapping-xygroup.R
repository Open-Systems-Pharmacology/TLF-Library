#' @title XYGDataMapping
#' @docType class
#' @description  Abstract class for X Y Group Mapping
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field groupMapping R6 class mapping groups to aesthetic properties
#' @section Methods:
#' \describe{
#' \item{new(x, y, groupMapping = NULL, color = NULL, fill = NULL, linetype = NULL, shape = NULL, size = NULL)}{
#' Initialize XYGDataMapping. Either input groupMapping or input color, fill, linetype, shape and/or size}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' }
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    groupMapping = NULL, # R6 Class of GroupMapping

    initialize = function(x, y = NULL,
                              groupMapping = NULL,
                              color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL) {
      super$initialize(x, y)

      validateEitherOrNullInput(groupMapping, list(
        "color" = color,
        "fill" = fill,
        "linetype" = linetype,
        "shape" = shape,
        "size" = size
      ))
      # To simplify the process workflow, groupMapping inputs color, fill... can be used directly instead of groupMapping
      self$groupMapping <- groupMapping %||% GroupMapping$new(
        color,
        fill,
        linetype,
        shape,
        size
      )
    },

    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data)
      validateMapping(self$y, data, nullAllowed = TRUE)

      # Drop option simplify data.frame into vectors
      # False enforces data to stay as data.frame if x or y is empty
      self$data <- data[, c(self$x, self$y), drop = FALSE]

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
