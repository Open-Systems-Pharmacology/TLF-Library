#' @title XYGDataMapping
#' @docType class
#' @description  Abstract class for X Y Group Mapping
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    groupMapping = NULL, # R6 Class of GroupMapping

    initialize = function(groupMapping = NULL, ...) {
      super$initialize(...)
      self$groupMapping <- groupMapping %||% GroupMapping$new()
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- cbind.data.frame(x, y)

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (!is.null(self$groupings[[groupType]]$group)) {
          grouping <- self$groupings[[groupType]]
          self$data[, grouping$label] <- grouping$getCaptions(data, metaData)
        }
      }
      return(self$data)
    }
  )
)
