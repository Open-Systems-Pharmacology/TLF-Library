#' @title HistogramDataMapping
#' @docType class
#' @description  Class for Histogram Group Mapping
#' @export
HistogramDataMapping <- R6::R6Class(
  "HistogramDataMapping",
  inherit = XDataMapping,
  public = list(
    groupings = NULL, # R6 Class of GroupMappings

    initialize = function(x, y, groupings = NULL) {
      super$initialize(x, y)

      self$groupings <- groupings %||% Groupings$new()
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x] 

      self$data <- data.frame(x)

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
