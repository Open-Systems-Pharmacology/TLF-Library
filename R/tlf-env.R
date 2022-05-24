# Environment that holds various global variables and settings for the tlf library,
# It is not exported and should not be directly manipulated by other packages.
tlfEnv <- new.env(parent = emptyenv())

# name of the package. This will be used to retrieve information on the package at run time
tlfEnv$packageName <- "tlf"

#' @title LegendPositions
#' @import ospsuite.utils
#' @description
#' List of all available legend positions
#' @family enum helpers
#' @export
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

#' @title TagPositions
#' @import ospsuite.utils
#' @description
#' List of all available tag positions in a plot grid.
#' @family enum helpers
#' @export
TagPositions <- enum(
  c(
    "topLeft" = "topleft",
    "top" = "top",
    "topRight" = "topright",
    "left" = "left",
    "right" = "right",
    "bottomLeft" = "bottomleft",
    "bottom" = "bottom",
    "bottomRight" = "bottomright"
  )
)

tlfEnv$defaultTagPosition <- TagPositions$topLeft

#' @title HorizontalJustification
#' @import ospsuite.utils
#' @description
#' List of all available horizontal justifications for plot annotation text.
#' @family enum helpers
#' @export
HorizontalJustification <- enum(
  c(
    "left" = 0,
    "middle" = 0.5,
    "right" = 1
  )
)

tlfEnv$defaultHorizontalJustification <- HorizontalJustification$left

#' @title VerticalJustification
#' @import ospsuite.utils
#' @description
#' List of all available vertical justifications for plot annotation text.
#' @family enum helpers
#' @export
VerticalJustification <- enum(
  c(
    "bottom" = 0,
    "middle" = 0.5,
    "top" = 1
  )
)

tlfEnv$defaultVerticalJustification <- VerticalJustification$bottom

#' @title setDefaultLegendPosition
#' @description Set default legend position of tlf environment
#' @param position legend position.
#' Use Enum `LegendPositions` to assess available legend positions
#' @export
setDefaultLegendPosition <- function(position) {
  validateIsIncluded(position, LegendPositions)
  # TO DO: Line to be deprecated
  tlfEnv$defaultLegendPosition <- position
  tlfEnv$currentTheme$background$legendPosition <- position
}

tlfEnv$defaultExportParameters <- list(
  format = "png",
  width = 16,
  height = 9,
  units = "cm",
  dpi = 300,
  name = "figure"
)

#' @title setDefaultExportParameters
#' @description Set default tlf properties for exporting/saving plots
#' @param format file format of the exported plots
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height`
#' @param dpi units of `width` and `height`
#' @param name base file name of the exported plots
#' @export
setDefaultExportParameters <- function(format = NULL, width = NULL, height = NULL, units = NULL, dpi = NULL, name = NULL) {
  validateIsIncluded(format, ExportFormats, nullAllowed = TRUE)
  validateIsIncluded(units, ExportUnits, nullAllowed = TRUE)
  validateIsNumeric(width, nullAllowed = TRUE)
  validateIsNumeric(height, nullAllowed = TRUE)
  validateIsNumeric(dpi, nullAllowed = TRUE)
  validateIsString(name, nullAllowed = TRUE)

  inputs <- c("format", "width", "height", "units", "dpi", "name")
  eval(parseVariableToObject(objectName = "tlfEnv$defaultExportParameters", inputs, keepIfNull = TRUE))
  return(invisible())
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
#' @param bins Number of bins if value, edges if vector or binning function if function
#' @export
#' @examples
#' # Set default number of bins
#' plotHistogram(x = rnorm(1000))
#'
#' setDefaultAggregationBins(21)
#' plotHistogram(x = rnorm(1000))
#'
setDefaultAggregationBins <- function(bins = NULL) {
  tlfEnv$defaultAggregation$bins <- bins %||% tlfEnv$defaultAggregation$bins
}

#' @title setDefaultWatermark
#' @description Set default watermark value for current theme
#' @param watermark A character value or `Label` object
#' @export
#' @examples
#' # Set default watermark using a character
#' setDefaultWatermark("Confidential")
#' addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))
#'
#' # Set default watermark using a `Label` object
#' setDefaultWatermark(Label$new(text = "Confidential", color = "red", angle = 30))
#' addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))
#'
setDefaultWatermark <- function(watermark = NULL) {
  validateIsOfType(watermark, c("Label", "character"), nullAllowed = TRUE)
  if (isOfType(watermark, "character")) {
    tlfEnv$currentTheme$background$watermark <- watermark
  }
  if (isOfType(watermark, "Label")) {
    tlfEnv$currentTheme$background$watermark <- watermark$text
    tlfEnv$currentTheme$fonts$watermark <- watermark$font
  }
  return(invisible())
}

tlfEnv$logTicks <- 10^seq(-6, 6)
tlfEnv$lnTicks <- exp(seq(-6, 6))

#' @title setDefaultLogTicks
#' @description Set default values for log ticks
#' @param ticks numeric values where ticks are placed.
#' Ensure that the values are positive (they are meant for log scale)
#' @export
setDefaultLogTicks <- function(ticks) {
  tlfEnv$logTicks <- ticks
  return(invisible())
}
