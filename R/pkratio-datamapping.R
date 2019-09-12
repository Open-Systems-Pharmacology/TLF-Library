#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYDataMapping,
  public = list(
    colorGrouping = NULL,
    sizeGrouping = NULL,
    shapeGrouping = NULL,

    # Example of how to do some other stuff
    initialize = function(x = "Age",
                          y = "Ratio",
                          colorGrouping = NULL,
                          sizeGrouping = NULL,
                          shapeGrouping = NULL) {
      self$colorGrouping <- colorGrouping
      self$sizeGrouping <- sizeGrouping
      self$shapeGrouping <- shapeGrouping

      super$initialize(x, y)
    },

    getGrouping = function(data, metaData) {
      data$colorGrouping <- getDefaultCaptions(data = data, metaData = metaData, variableList = self$colorGrouping)
      data$sizeGrouping <- getDefaultCaptions(data = data, metaData = metaData, variableList = self$sizeGrouping)
      data$shapeGrouping <- getDefaultCaptions(data = data, metaData = metaData, variableList = self$shapeGrouping)

      return(data)
    }
  )
)

#' @title getDefaultCaptions
#' @param data input data.frame with variables to group by
#' @param metaData input data.frame with variables to group by
#' @param variableList groups as factor levels
#' @description
#' createGroupingVariable create a new column that groups the grouping variable
#' @return groupingVariable
#' @export
#'
getDefaultCaptions <- function(data, metaData, variableList = colnames(data)) {

  # Check that the grouping is in the list of data variables
  stopifnot(variableList %in% colnames(data))

  groupingVariable <- asLegendCaptionSubset(
    data[, variableList[1]],
    metaData[[variableList[1]]]
  )

  # Paste the consecutively the variable names
  for (variable in tail(variableList, -1)) {
    groupingVariable <- paste(
      groupingVariable,
      asLegendCaptionSubset(
        data[, variable],
        metaData[[variable]]
      ), sep = "-"
    )
  }

  if (length(groupingVariable) == 0) {
    groupingVariable <- 1
  }
  groupingVariable <- as.factor(groupingVariable)
  return(groupingVariable)
}


asLegendCaptionSubset <- function(data, metaData) {
  CaptionSubset <- as.character(data)

  # If numeric create a character as rounded numeric + unit from metadata
  if ("numeric" %in% class(data)) {
    CaptionSubset <- paste(as.character(round(data)), metaData$Unit, sep = "")
  }

  return(CaptionSubset)
}
