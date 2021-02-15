#' @title Scaling
#' @include enum.R
#' @export
#' @description
#'  Pre-defined transformation of axes
#'  Not that built-in transformations from `ggplot2` includes more transformations
Scaling <- enum(c(
  "lin", "log", "discrete",
  "reverse", "sqrt", "time", "date"
))

#' @title createPlotScale
#' @description Translate scale into a value directly usable by `ggplot2`
#' to give more flexibilty in the next functions
#' @param scale character defining the name of the scale
#' @return name of the `ggplot2` scale
createPlotScale <- function(scale) {
  validateIsString(scale)
  if (isIncluded(tolower(scale), c("identity", "lin", "linear", "default", "normal"))) {
    return("identity")
  }
  if (isIncluded(tolower(scale), c("log", "logarithmic", "log10"))) {
    return("log10")
  }
  validateIsIncluded(tolower(scale), Scaling)
  return(tolower(scale))
}

#' @title createPlotTicks
#' @description Translate ticks and ticklabels into a value directly usable by `ggplot2`
#' to give more flexibilty in the next functions
#' @param ticks character, numeric or function defining the ticks
#' @return name of the `ggplot2` scale
createPlotTicks <- function(ticks) {
  if (isOfLength(ticks, 0)) {
    return(waiver())
  }
  if (isIncluded(ticks, c("default", "identity"))) {
    return(waiver())
  }
  if (isIncluded(ticks, c("none"))) {
    return(NULL)
  }
  if (isOfType(ticks, c("numeric", "character", "function"))) {
    return(ticks)
  }
}


#' @title AxisConfiguration
#' @description  R6 class defining the configuration of axis
#' @export
AxisConfiguration <- R6::R6Class(
  "AxisConfiguration",
  public = list(
    #' @description Create a new \code{AxisConfiguration} object
    #' @param limits numeric vector of axis limits
    #' @param scale character defining axis scale
    #' Use enum `Scaling` to access predefined scales.
    #' @param ticks numeric vector or function defining where to position axis ticks
    #' @param ticklabels character vector or function defining what to print on axis ticks
    #' @param font \code{Font} object defining the font of ticklabels
    #' @return A new \code{AxisConfiguration} object
    initialize = function(limits = NULL,
                              scale = Scaling$lin,
                              ticks = NULL,
                              ticklabels = NULL,
                              font = NULL) {
      validateIsNumeric(limits, nullAllowed = TRUE)
      validateIsOfType(font, "Font", nullAllowed = TRUE)
      private$.limits <- limits

      scale <- scale %||% Scaling$lin
      private$.scale <- createPlotScale(scale)
      private$.ticks <- createPlotTicks(ticks)
      private$.ticklabels <- createPlotTicks(ticklabels)

      # Default axis font will use theme
      defaultFont <- Font$new()
      if (isOfType(self, "XAxisConfiguration")) {
        defaultFont <- tlfEnv$currentTheme$fonts$xAxis
      }
      if (isOfType(self, "YAxisConfiguration")) {
        defaultFont <- tlfEnv$currentTheme$fonts$yAxis
      }
      private$.font <- font %||% defaultFont
    }
  ),
  active = list(
    #' @field limits numeric vector of length 2 defining limits of axis.
    #' A value of `NULL` is allowed and lead to default `ggplot2` behaviour
    limits = function(value) {
      if (missing(value)) {
        return(private$.limits)
      }
      validateIsNumeric(value, nullAllowed = TRUE)
      if (isOfLength(value, 0)) {
        private$.limits <- NULL
        return(invisible())
      }
      validateIsOfLength(value, 2)
      private$.limits <- value
      return(invisible())
    },
    #' @field scale name of axis scale from Enum `Scaling`
    #' A value of `NULL` is allowed and will lead to a default linear scale
    scale = function(value) {
      if (missing(value)) {
        return(private$.scale)
      }
      value <- value %||% Scaling$lin
      private$.scale <- createPlotScale(value)
      return(invisible())
    },
    #' @field ticks function or values defining where axis ticks are placed
    ticks = function(value) {
      if (missing(value)) {
        return(private$.ticks)
      }
      private$.ticks <- createPlotTicks(value)
      return(invisible())
    },
    #' @field ticklabels function or values defining the axis tick labels
    ticklabels = function(value) {
      if (missing(value)) {
        return(private$.ticklabels)
      }
      private$.ticklabels <- createPlotTicks(value)
      return(invisible())
    },
    #' @field font \code{Font} object defining the font of the ticklabels
    font = function(value) {
      if (missing(value)) {
        return(private$.font)
      }
      validateIsOfType(value, "Font", nullAllowed = TRUE)
      # Default axis font will use theme
      defaultFont <- Font$new()
      if (isOfType(self, "XAxisConfiguration")) {
        defaultFont <- tlfEnv$currentTheme$fonts$xAxis
      }
      if (isOfType(self, "YAxisConfiguration")) {
        defaultFont <- tlfEnv$currentTheme$fonts$yAxis
      }
      private$.font <- value %||% defaultFont
      return(invisible())
    }
  ),
  private = list(
    .limits = NULL,
    .scale = NULL,
    .ticks = NULL,
    .ticklabels = NULL,
    .font = NULL
  )
)

#' @title XAxisConfiguration
#' @description  R6 class defining the configuration of X-axis
#' @export
XAxisConfiguration <- R6::R6Class(
  "XAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    #' @description Update axis configuration on a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object with updated axis properties
    updatePlot = function(plotObject) {
      validateIsOfType(plotObject, "ggplot")
      # Update font properties
      plotObject <- plotObject + ggplot2::theme(axis.text.x = private$.font$createPlotFont())
      # Update scales and ticks
      if (isIncluded(private$.scale, Scaling$discrete)) {
        suppressMessages(
          plotObject <- plotObject +
            ggplot2::scale_x_discrete(limits = private$.limits, breaks = private$.ticks, labels = private$.ticklabels)
        )
        return(plotObject)
      }
      # Most of ggplot2 scales lead to unwanted warning messages
      # `try` should be added in cases of scale breaking because all the ggplot object elements are not yet in place
      suppressMessages(
        plotObject <- plotObject +
          ggplot2::scale_x_continuous(trans = private$.scale, limits = private$.limits, breaks = private$.ticks, labels = private$.ticklabels)
      )
      return(plotObject)
    }
  )
)

#' @title YAxisConfiguration
#' @description  R6 class defining the configuration of Y-axis
#' @export
YAxisConfiguration <- R6::R6Class(
  "YAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    #' @field position character poistion of the Y-axis
    position = NULL, # TO DO: find a way to include position in y axis, then scale position = "left" or "right"

    #' @description Update axis configuration on a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object with updated axis properties
    updatePlot = function(plotObject) {
      validateIsOfType(plotObject, "ggplot")
      # Update font properties
      plotObject <- plotObject + ggplot2::theme(axis.text.y = private$.font$createPlotFont())
      # Update scales and ticks
      if (isIncluded(private$.scale, Scaling$discrete)) {
        suppressMessages(
          plotObject <- plotObject +
            ggplot2::scale_y_discrete(limits = private$.limits, breaks = private$.ticks, labels = private$.ticklabels)
        )
        return(plotObject)
      }
      # Most of ggplot2 scales lead to unwanted warning messages
      # `try` should be added in cases of scale breaking because all the ggplot object elements are not yet in place
      suppressMessages(
        plotObject <- plotObject +
          ggplot2::scale_y_continuous(trans = private$.scale, limits = private$.limits, breaks = private$.ticks, labels = private$.ticklabels)
      )
      return(plotObject)
    }
  )
)
