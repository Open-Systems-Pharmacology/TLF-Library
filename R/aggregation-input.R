#' @title AggregationInput
#' @docType class
#' @description  #Class to be used to construct inputs to the AggregationSummary class
#' @export
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
  "mean-sd", "mean+sd", "mean-1.96sd", "mean+1.96sd",
  sapply(predefinedPercentiles, function(percentileValue) {
    paste0("Percentile", percentileValue, "%")
  }),
  "median-IQR", "median+IQR", "median-1.5IQR", "median+1.5IQR",
  "Percentile25%-1.5IQR", "Percentile75%+1.5IQR"
))

#' @export
`mean-1.96sd` <- function(x) {
  return(mean(x) - 1.96 * stats::sd(x))
}

#' @export
`mean+1.96sd` <- function(x) {
  return(mean(x) + 1.96 * stats::sd(x))
}

#' @export
`mean-sd` <- function(x) {
  return(mean(x) - stats::sd(x))
}

#' @export
`mean+sd` <- function(x) {
  return(mean(x) + stats::sd(x))
}

#' @export
`median-IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] - (quartiles[2] - quartiles[3])
  return(as.numeric(res))
}

#' @export
`median+IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] + (quartiles[2] - quartiles[3])
  return(as.numeric(res))
}

#' @export
`median-1.5IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] - 1.5 * (quartiles[2] - quartiles[3])
  return(as.numeric(res))
}

#' @export
`median+1.5IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.5, 0.75, 0.25))
  res <- quartiles[1] + 1.5 * (quartiles[2] - quartiles[3])
  return(as.numeric(res))
}

#' @export
`Percentile25%-1.5IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.75, 0.25))
  res <- quartiles[2] - 1.5 * (quartiles[1] - quartiles[2])
  return(as.numeric(res))
}

#' @export
`Percentile75%+1.5IQR` <- function(x) {
  quartiles <- stats::quantile(x, probs = c(0.75, 0.25))
  res <- quartiles[1] + 1.5 * (quartiles[1] - quartiles[2])
  return(as.numeric(res))
}

# Assign did not work because the functions need to be documented with @export
# in order to be used, however I did not find the way to do it using assign
# I left the code as a comment in case it may be re-used
# for (percentileValue in predefinedPercentiles) {
#  # Use eval and parse to use evaluated values within functions
#  percentileFunction <- eval(parse(text = paste0("function (x) {as.numeric(stats::quantile(x, probs = ", percentileValue / 100, "))}")))
#  assign(paste0("Percentile-", percentileValue, "%"), percentileFunction)
# }

#' @export
`Percentile0%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 0))
}

#' @export
`Percentile1%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 1 / 100))
}

#' @export
`Percentile2.5%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 2.5 / 100))
}

#' @export
`Percentile5%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 5 / 100))
}

#' @export
`Percentile10%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 10 / 100))
}

#' @export
`Percentile15%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 15 / 100))
}

#' @export
`Percentile20%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 20 / 100))
}

#' @export
`Percentile25%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 25 / 100))
}

#' @export
`Percentile50%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 50 / 100))
}

#' @export
`Percentile75%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 75 / 100))
}

#' @export
`Percentile80%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 80 / 100))
}

#' @export
`Percentile85%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 85 / 100))
}

#' @export
`Percentile90%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 90 / 100))
}

#' @export
`Percentile95%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 95 / 100))
}

#' @export
`Percentile97.5%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 97.5 / 100))
}

#' @export
`Percentile99%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 99 / 100))
}

#' @export
`Percentile100%` <- function(x) {
  as.numeric(stats::quantile(x, probs = 1))
}
