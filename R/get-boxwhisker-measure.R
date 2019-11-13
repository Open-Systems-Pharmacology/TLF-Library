#' get summary table of Box Whisker plot
#'
#' @title getBoxWhiskerMeasure
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param dataMapping R6 class BoxWhiskerDataMapping
#' mapping of x, y and groupings
#' @param ratioLimits vector of numeric
#' containing the fold limits of PK ratios
#' As default limits are 1.5 and 2 folds
#' @description
#' getPKRatioMeasure(data, dataMapping, ratioLimits) get the number of PK ratios
#' that are within specific limits
#' @return a data.frame reporting the PK Ratio Qualification Measure
#' @export
#'
getBoxWhiskerMeasure <- function(data, dataMapping = NULL, y = NULL, group = NULL, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95)) {

  # If no data mapping is input, use default
  variableNames <- names(data)
  group <- group %||% head(variableNames, 1)
  y <- y %||% tail(variableNames, 1)
  dataMapping <- dataMapping %||% BoxWhiskerDataMapping$new(
    data = data,
    x = group,
    y = y
  )

  validateIsOfType(dataMapping, "BoxWhiskerDataMapping")

  # Redfine group and y while removing NA values
  group <- data[!is.na.data.frame(data[, dataMapping$y]), dataMapping$x]
  y <- data[!is.na.data.frame(data[, dataMapping$y]), dataMapping$y]

  summary <- tapply(y, group, FUN = function(x) {
    summaryStat(x, quantiles)
  })
  # As a data.frame summary row names are already group names
  summary <- as.data.frame(t(sapply(summary, FUN = rbind)))

  # Re-label variables
  names(summary) <- c(
    "N",
    sapply(quantiles, function(x) {
      paste0(100 * x, "th percentile")
    }),
    "mean",
    "standard deviation",
    "geo mean",
    "geo standard deviation"
  )


  return(summary)
}


summaryStat <- function(x, quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = FALSE) {
  n <- length(x)
  summaryQuantiles <- as.numeric(quantile(x, probs = quantiles))
  summaryStat <- c(mean(x, na.rm = na.rm), sd(x, na.rm = na.rm), exp(mean(log(x), na.rm = na.rm)), exp(sd(log(x), na.rm = na.rm)))

  summary <- c(n, summaryQuantiles, summaryStat)

  return(summary)
}
