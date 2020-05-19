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
