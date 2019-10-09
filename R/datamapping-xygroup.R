#' @title XYGDataMapping
#' @docType class
#' @description  Abstract class for X Y Group Mapping
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    groupings = NULL, # List of Groupings that are R6 Classes as defined by Abdullah
    groupingNames = NULL,

    initialize = function(x, y, groupings = NULL, groupingNames = NULL) {
      super$initialize(x, y)
      self$groupings <- groupings

      if (is.character(groupings)) {
        groupings <- list(groupings)
      }

      self$groupingNames <- groupingNames %||% names(groupings)

      if (!is.null(groupings)) {
        for (groupingsIndex in seq(1, length(groupings))) {
          if (is.null(self$groupingNames[groupingsIndex]) || is.na(self$groupingNames[groupingsIndex])) {
            self$groupingNames[groupingsIndex] <- paste(groupings[[groupingsIndex]], collapse = "-")
          }

          self$groupings[[groupingsIndex]] <- Grouping$new(groupings[groupingsIndex])

          # TODO Make sure it works with New grouping class
          # self$groupings[[groupingsIndex]] <- Grouping$new(
          #   group = groupings[[groupingsIndex]],
          #   groupName = self$groupingNames[groupingsIndex],
          #   data = self$data,
          #   metaData = self$metaData
          # )
        }
        names(self$groupings) <- self$groupingNames
      }
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- cbind.data.frame(x, y)

      # For each mapped grouping, get the corresponding data frame
      # Defined by grouping data frame if available or by grouping Name
      if (!is.null(self$groupings)) {
        for (groupingsIndex in seq(1, length(self$groupings))) {
          self$data[, self$groupingNames[[groupingsIndex]]] <-  getCustomCaptions(data, self$groupings[[groupingsIndex]]$groupingDataFrame) %||% getDefaultCaptions(data, metaData, variableList = self$groupings[[groupingsIndex]]$groupingName[[1]])
        }
      }

      return(self$data)
    }
  )
)
