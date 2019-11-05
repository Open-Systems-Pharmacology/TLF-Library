#' @title AggregationInput
#' @docType class
#' @description  #Class to be used to construct inputs to the AggregationSummary class
#'  @export


AggregationInput <- R6::R6Class(
  "AggregationInput",
  public = list(
    aggregationFunction = NULL,
    aggregationFunctionName = NULL,
    aggregationUnit = NULL,
    aggregationDimension = NULL,
    initialize = function(aggregationFunction = NULL,
                          aggregationFunctionName = NULL,
                          aggregationUnit = NULL,
                          aggregationDimension = NULL) {
      self$aggregationFunction = aggregationFunction
      self$aggregationFunctionName = aggregationFunctionName
      self$aggregationUnit = aggregationUnit
      self$aggregationDimension = aggregationDimension
    }
  )
)
