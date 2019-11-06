#' @title XYGDataMapping
#' @docType class
#' @description  Abstract class for X Y Group Mapping
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    groupMapping = NULL, # R6 Class of GroupMapping

    initialize = function(..., groupMapping = NULL) {
      super$initialize(...)
      self$groupMapping <- groupMapping %||% GroupMapping$new()
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- cbind.data.frame(x, y)

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (!is.null(self$groupingMapping[[groupType]]$group)) {
          grouping <- self$groupingMapping[[groupType]]
          self$data[, grouping$label] <- grouping$getCaptions(data, metaData)
        }
      }
      return(self$data)
    },

    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data)
      validateMapping(self$y, data)

      self$data <- data[, c(self$x, self$y)]

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
