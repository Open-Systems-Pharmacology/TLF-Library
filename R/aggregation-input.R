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
      self$aggregationFunction <- aggregationFunction
      self$aggregationFunctionName <- aggregationFunctionName
      self$aggregationUnit <- aggregationUnit
      self$aggregationDimension <- aggregationDimension
    }
  )
)

predefinedPercentiles <- c(0, 1, 2.5, 5, 10, 15, 20, 25, 50, 75, 80, 85, 90, 95, 97.5, 99, 100)
#' @title tlfStatFunctions
#' @description
#' Bank of predifined functions ready to use by Aggregation methods. Bank defined as Enum.
#' To access the function from its name, use match.fun: e.g. testFun <- match.fun("mean-1.96sd")
#' @include enum.R
#' @export
tlfStatFunctions <- enum(c(
  "mean", "sd",
  "min", "max",
  "mean-1.96sd", "mean-1.96sd",
  sapply(predefinedPercentiles, function(percentileValue) {
    paste0("Percentile-", percentileValue, "%")
  }),
  "median-IQR", "median+IQR", "median-1.5IQR", "median+1.5IQR"
))

# Use assign to define a function with a specific name
for (percentileValue in predefinedPercentiles) {
  # Use eval and parse to use evaluated values within functions
  percentileFunction <- eval(parse(text = paste0("function (x) {as.numeric(stats::quantile(x, probs = ", percentileValue / 100, "))}")))
  assign(paste0("Percentile-", percentileValue, "%"), percentileFunction)
}

assign("mean-1.96sd", function(x) {
  return(mean(x) - 1.96 * stats::sd(x))
})
assign("mean+1.96sd", function(x) {
  return(mean(x) + 1.96 * stats::sd(x))
})

assign("median-IQR", function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] - (quartiles[2] - quartiles[3])
  return(res)
})
assign("median+IQR", function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] + (quartiles[2] - quartiles[3])
  return(res)
})
assign("median-1.5IQR", function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] - 1.5 * (quartiles[2] - quartiles[3])
  return(res)
})
assign("median+1.5IQR", function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] + 1.5 * (quartiles[2] - quartiles[3])
  return(res)
})
