#' @title Grouping
#' @docType class
#' @description  Abstract class for Grouping
#' @export
Grouping <- R6::R6Class(
  "Grouping",
  public = list(
    groupingName = NULL,
    groupingDataFrame = NULL,
    groupingLegendTitle = NULL,

    initialize = function(groupingName = NULL, groupingDataFrame = NULL, groupingLegendTitle = NULL) {
      self$groupingLegendTitle <- groupingLegendTitle
      groupingList <- self$getGroupingList(groupingName, groupingDataFrame)
      self$groupingName <- groupingList$selfGrouping
      self$groupingDataFrame <- groupingList$selfGroupingDataFrame
    },

    getGroupingList = function(groupingName, groupingDataFrame) { # move out of public?
      if (!is.null(groupingDataFrame)) {
        selfGroupingDataFrame <- groupingDataFrame
        selfGrouping <- groupingName %||% paste(head(colnames(groupingDataFrame), -1), collapse = "-") # the last column heading of the grouping dataframe will be a default legend title if no groupingLegendTitle is provided
      } else {
        selfGroupingDataFrame <- NULL
        selfGrouping <- groupingName
      }
      groupingList <- list(selfGroupingDataFrame = selfGroupingDataFrame, selfGrouping = selfGrouping)
      return(groupingList)
    }
  )
)
