# Environment that holds various global variables and settings for the tlf library,
# It is not exported and should not be directly manipulated by other packages.
tlfEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
tlfEnv$packageName <- "tlf"

# Set the current theme of the tlf plot configurations
tlfEnv$currentTheme <- defaultTheme

#' @title LegendPositions
#' @include enum.R
#' @export
#' @description
#' List of all available legend positions
LegendPositions <- enum(c(
  "none",
  "insideTop",
  "insideTopLeft",
  "insideLeft",
  "insideBottomLeft",
  "insideBottom",
  "insideBottomRight",
  "insideRight",
  "insideTopRight",
  "outsideTop",
  "outsideTopLeft",
  "outsideLeft",
  "outsideBottomLeft",
  "outsideBottom",
  "outsideBottomRight",
  "outsideRight",
  "outsideTopRight"
))

tlfEnv$defaultLegendPosition <- LegendPositions$outsideRight

#' @title setDefaultLegendPosition
#' @description Set default legend position of tlf environment
#' @param position legend position.
#' Use Enum `LegendPositions` to assess available legend positions
#' @export
setDefaultLegendPosition <- function(position) {
  validateIsIncluded(position, LegendPositions)
  tlfEnv$defaultLegendPosition <- position
}

tlfEnv$defaultExportParameters <- list(format = "png", width = 16, height = 9, units = "cm")

#' @title setDefaultSaveParameters
#' @description Set default save properties of tlf environment
#' @param format file format of the exported plots
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height`
#' @export
setDefaultExportParameters <- function(format, width = NULL, height = NULL, units = NULL) {
  validateIsString(format)
  validateIsNumeric(c(width, height), nullAllowed = TRUE)
  validateIsString(units, nullAllowed = TRUE)

  tlfEnv$defaultExportParameters$format <- format
  tlfEnv$defaultExportParameters$width <- width %||% tlfEnv$defaultExportParameters$width
  tlfEnv$defaultExportParameters$height <- height %||% tlfEnv$defaultExportParameters$height
  tlfEnv$defaultExportParameters$units <- units %||% tlfEnv$defaultExportParameters$units
}

tlfEnv$defaultExportName <- "tlf-plot"

#' @title setDefaultExportName
#' @description Set default save name property of tlf environment
#' @param name base file name of the exported plots
#' @export
setDefaultExportName <- function(name) {
  validateIsString(name)
  tlfEnv$defaultExportName$name <- name
}

tlfEnv$defaultAggregation <- list(
  functions = list(
    y = median,
    ymin = function(x) {
      as.numeric(stats::quantile(x, probs = 5 / 100))
    },
    ymax = function(x) {
      as.numeric(stats::quantile(x, probs = 95 / 100))
    }
  ),
  labels = list(
    y = "median",
    range = "[5th-95th] percentiles"
  ),
  bins = 10
)

#' @title setDefaultAggregationFunctions
#' @description Set default aggregation functions of tlf environment
#' @param y function or its name as median aggregation
#' @param ymin function or its name as min aggregation
#' @param ymax function or its name as max aggregation
#' @export
setDefaultAggregationFunctions <- function(y = NULL, ymin = NULL, ymax = NULL) {
  validateIsOfType(c(y, ymin, ymax), c("function", "character"), nullAllowed = TRUE)

  if (isOfType(y, "character")) {
    y <- match.fun(y)
  }
  if (isOfType(ymin, "character")) {
    ymin <- match.fun(ymin)
  }
  if (isOfType(ymax, "character")) {
    ymax <- match.fun(ymax)
  }

  tlfEnv$defaultAggregation$functions$y <- y %||% tlfEnv$defaultAggregation$functions$y
  tlfEnv$defaultAggregation$functions$ymin <- ymin %||% tlfEnv$defaultAggregation$functions$ymin
  tlfEnv$defaultAggregation$functions$ymax <- ymax %||% tlfEnv$defaultAggregation$functions$ymax
}

#' @title setDefaultAggregationLabels
#' @description Set default aggregation labels of tlf environment
#' @param y label for median aggregation
#' @param range label for range aggregation
#' @export
setDefaultAggregationLabels <- function(y = NULL, range = NULL) {
  validateIsString(c(y, range), nullAllowed = TRUE)

  tlfEnv$defaultAggregation$labels$y <- y %||% tlfEnv$defaultAggregation$labels$y
  tlfEnv$defaultAggregation$labels$range <- range %||% tlfEnv$defaultAggregation$labels$range
}

#' @title setDefaultAggregationBins
#' @description Set default aggregation bins of tlf environment
#' @param bins number of bins if value, edges if vector or binning function if function
#' @export
setDefaultAggregationBins <- function(bins = NULL) {
  tlfEnv$defaultAggregation$bins <- bins %||% tlfEnv$defaultAggregation$bins
}
