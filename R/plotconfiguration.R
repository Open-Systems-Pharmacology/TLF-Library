#' @title PlotConfiguration
#' @description R6 class defining the configuration of a `ggplot` object
#' @field export R6 class `ExportConfiguration` defining export properties
#' @family PlotConfiguration classes
#' @references For examples, see:
#' \link{https://www.open-systems-pharmacology.org/TLF-Library/articles/plot-configuration.html}
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  public = list(
    export = NULL,

    #' @description Create a new `PlotConfiguration` object
    #' @param title character or `Label` object defining plot title
    #' @param subtitle character or `Label` object defining plot subtitle
    #' @param xlabel character or `Label` object defining plot xlabel
    #' @param ylabel character or `Label` object defining plot ylabel
    #' @param legend `LegendConfiguration` object defining legend properties
    #' @param legendTitle character defining legend title
    #' @param legendPosition character defining legend position.
    #' Use Enum `LegendPositions` to get a list of available to legend positions.
    #' @param xAxis `XAxisConfiguration` object defining x-axis properties
    #' @param xScale name of X-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param xLimits numeric vector of length 2 defining x-axis limits
    #' @param yAxis `YAxisConfiguration` object defining y-axis properties
    #' @param yScale name of y-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param yLimits numeric vector of length 2 defining y-axis limits
    #' @param background `BackgroundConfiguration` object defining background properties
    #' @param plotArea `BackgroundElement` object defining properties of plot area
    #' @param panelArea `BackgroundElement` object defining properties of panel area
    #' @param xGrid `LineElement` object defining properties of x-grid background
    #' @param yGrid `LineElement` object defining properties of y-grid background
    #' @param watermark `Label` object defining watermark
    #' @param export R6 class `SaveConfiguration` defining saving properties
    #' @param format character defining the format of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param data data.frame used by `smartMapping`
    #' @param metaData list of information on `data`
    #' @param dataMapping R6 class or subclass `XYDataMapping`
    #' @return A new `PlotConfiguration` object
    initialize = function( # Label configuration
                          title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          # Legend Configuration
                          legend = NULL,
                          legendTitle = NULL,
                          legendPosition = NULL,
                          # X-Axis configuration
                          xAxis = NULL,
                          xScale = NULL,
                          xLimits = NULL,
                          # Y-Axis configuration
                          yAxis = NULL,
                          yScale = NULL,
                          yLimits = NULL,
                          # Background configuration
                          background = NULL,
                          plotArea = NULL,
                          panelArea = NULL,
                          xGrid = NULL,
                          yGrid = NULL,
                          watermark = NULL,
                          # Export configuration
                          export = NULL,
                          format = NULL,
                          width = NULL,
                          height = NULL,
                          units = NULL,
                          # Smart configuration using metaData
                          data = NULL,
                          metaData = NULL,
                          dataMapping = NULL) {

      # Label configuration
      # validation of the input is done within the creation of the object
      private$.labels <- LabelConfiguration$new(
        title = title, subtitle = subtitle,
        xlabel = xlabel, ylabel = ylabel
      )

      # Smart configuration if xlabel and ylabel
      # 1) If xlabel and ylabel provided: use as is.
      # 2) Else, if data, metaData (and optionally dataMapping) provided: use dimension [unit] from metaData
      # 3) Else, if data (and optionally dataMapping) provided: use variable names from data
      if (!is.null(data)) {
        dataMapping <- dataMapping %||% XYGDataMapping$new(data = data)
      }
      private$.labels$xlabel <- asLabel(xlabel %||%
        dataMappingLabel(dataMapping$x, metaData) %||%
        dataMapping$x %||%
        private$.labels$xlabel$text, font = private$.labels$xlabel$font)
      private$.labels$ylabel <- asLabel(ylabel %||%
        dataMappingLabel(dataMapping$y, metaData) %||%
        dataMapping$y %||%
        dataMappingLabel(dataMapping$ymax, metaData) %||%
        dataMapping$ymax %||%
        private$.labels$ylabel, font = private$.labels$ylabel$font)

      # Legend Configuration, overwrite some properties only if they are defined
      validateIsOfType(legend, "LegendConfiguration", nullAllowed = TRUE)
      private$.legend <- legend %||% LegendConfiguration$new()
      private$.legend$title <- legendTitle %||% private$.legend$title
      validateIsIncluded(legendPosition, LegendPositions, nullAllowed = TRUE)
      private$.legend$position <- legendPosition %||% private$.legend$position

      # X-Axis Configuration, overwrite some properties only if they are defined
      validateIsOfType(xAxis, "XAxisConfiguration", nullAllowed = TRUE)
      private$.xAxis <- xAxis %||% XAxisConfiguration$new(scale = xAxisDefaultScale(self))
      private$.xAxis$limits <- xLimits %||% private$.xAxis$limits
      private$.xAxis$scale <- xScale %||% private$.xAxis$scale

      # Y-Axis configuration, overwrite some properties only if they are defined
      validateIsOfType(yAxis, "YAxisConfiguration", nullAllowed = TRUE)
      private$.yAxis <- yAxis %||% YAxisConfiguration$new(scale = yAxisDefaultScale(self))
      private$.yAxis$limits <- yLimits %||% private$.yAxis$limits
      private$.yAxis$scale <- yScale %||% private$.yAxis$scale

      # Background configuration, overwrite some properties only if they are defined
      validateIsOfType(background, "BackgroundConfiguration", nullAllowed = TRUE)
      validateIsOfType(plotArea, "BackgroundElement", nullAllowed = TRUE)
      validateIsOfType(panelArea, "BackgroundElement", nullAllowed = TRUE)
      validateIsOfType(xGrid, "LineElement", nullAllowed = TRUE)
      validateIsOfType(yGrid, "LineElement", nullAllowed = TRUE)
      validateIsOfType(watermark, c("character", "Label"), nullAllowed = TRUE)

      private$.background <- background %||% BackgroundConfiguration$new()
      private$.background$plot <- plotArea %||% private$.background$plot
      private$.background$panel <- panelArea %||% private$.background$panel
      private$.background$xGrid <- xGrid %||% private$.background$xGrid
      private$.background$yGrid <- yGrid %||% private$.background$yGrid
      private$.background$watermark <- watermark %||% private$.background$watermark

      # Define atom behaviour from theme
      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      private$.lines <- asThemeAestheticSelections(currentTheme$plotConfigurations$addLine)
      private$.ribbons <- asThemeAestheticSelections(currentTheme$plotConfigurations$addRibbon)
      private$.points <- asThemeAestheticSelections(currentTheme$plotConfigurations$addScatter)
      private$.errorbars <- asThemeAestheticSelections(currentTheme$plotConfigurations$addErrorbar)

      # Define export configuration, overwrite properties only if they are defined
      self$export <- export %||% ExportConfiguration$new()
      self$export$format <- format %||% self$export$format
      self$export$width <- width %||% self$export$width
      self$export$height <- height %||% self$export$height
      self$export$units <- units %||% self$export$units
    }
  ),
  active = list(
    #' @field labels `LabelConfiguration` object defining properties of labels
    labels = function(value) {
      if (missing(value)) {
        return(private$.labels)
      }
      validateIsOfType(value, "LabelConfiguration", nullAllowed = TRUE)
      private$.labels <- value %||% private$.labels
      return(invisible())
    },
    #' @field legend `LegendConfiguration` object defining properties of legend
    legend = function(value) {
      if (missing(value)) {
        return(private$.legend)
      }
      validateIsOfType(value, "LegendConfiguration", nullAllowed = TRUE)
      private$.legend <- value %||% private$.legend
      return(invisible())
    },
    #' @field xAxis `XAxisConfiguration` object defining properties of x-axis
    xAxis = function(value) {
      if (missing(value)) {
        return(private$.xAxis)
      }
      validateIsOfType(value, "XAxisConfiguration", nullAllowed = TRUE)
      private$.xAxis <- value %||% private$.xAxis
      return(invisible())
    },
    #' @field yAxis `YAxisConfiguration` object defining properties of x-axis
    yAxis = function(value) {
      if (missing(value)) {
        return(private$.yAxis)
      }
      validateIsOfType(value, "YAxisConfiguration", nullAllowed = TRUE)
      private$.yAxis <- value %||% private$.yAxis
      return(invisible())
    },
    #' @field background `BackgroundConfiguration` object defining properties of x-axis
    background = function(value) {
      if (missing(value)) {
        return(private$.background)
      }
      validateIsOfType(value, "BackgroundConfiguration", nullAllowed = TRUE)
      private$.background <- value %||% private$.background
      return(invisible())
    },
    #' @field lines `ThemeAestheticSelections` defining properties of lines
    lines = function(value) {
      if (missing(value)) {
        return(private$.lines)
      }
      validateIsOfType(value, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.lines <- value %||% private$.lines
      return(invisible())
    },
    #' @field ribbons `ThemeAestheticSelections` defining properties of ribbons
    ribbons = function(value) {
      if (missing(value)) {
        return(private$.ribbons)
      }
      validateIsOfType(value, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.ribbons <- value %||% private$.ribbons
      return(invisible())
    },
    #' @field points `ThemeAestheticSelections` defining properties of points
    points = function(value) {
      if (missing(value)) {
        return(private$.points)
      }
      validateIsOfType(value, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.points <- value %||% private$.points
      return(invisible())
    },
    #' @field errorbars `ThemeAestheticSelections` defining properties of error bars
    errorbars = function(value) {
      if (missing(value)) {
        return(private$.errorbars)
      }
      validateIsOfType(value, "ThemeAestheticSelections", nullAllowed = TRUE)
      private$.errorbars <- value %||% private$.errorbars
      return(invisible())
    }
  ),
  private = list(
    .labels = NULL,
    .legend = NULL,
    .xAxis = NULL,
    .yAxis = NULL,
    .background = NULL,
    .lines = NULL,
    .ribbons = NULL,
    .points = NULL,
    .errorbars = NULL
  )
)


## -------------------------------------------------------------------
# List of auxiliary functions used by plotConfiguration

#' @title dataMappingLabel
#' @param mapping mapping variable on label
#' @param metaData metaData containing unit of variable
#' @return label character of variable with its unit
#' @description
#' dataMappingLabel create an axis label with unit if available
#' @keywords internal
dataMappingLabel <- function(mapping = NULL, metaData = NULL) {
  label <- NULL
  ifnotnull(mapping, label <- getLabelWithUnit(metaData[[mapping]]$dimension, metaData[[mapping]]$unit))

  return(label)
}

#' @title getLabelWithUnit
#' @param label text of axis label
#' @param unit metaData containing unit of variable
#' @return label character of variable with its unit when available
#' @description
#' getLabelWithUnit paste unit to label when available
#' @export
getLabelWithUnit <- function(label, unit = NULL) {
  if (isTRUE(unit %in% "")) {
    unit <- NULL
  }
  ifnotnull(unit, label <- paste(label, " [", unit, "]", sep = ""))
  return(label)
}

#' @title getLegendCaption
#' @param captionData data used for legend caption
#' @param which caption position in the legend
#' @return legend captions
#' @description
#' getLegendCaption return the legend captions or a subset using which
#' @export
getLegendCaption <- function(captionData, which = seq(1, length(levels(captionData)))) {
  return(levels(captionData)[which])
}

#' @title updateLegendCaption
#' @param captionData data used for legend caption
#' @param newCaption new legend caption
#' @param which caption position in the legend
#' @return updated caption data
#' @description
#' updateLegendCaption return the updated data used for legend caption
#' @export
updateLegendCaption <- function(captionData, newCaption = NULL, which = NULL) {
  ifnotnull(which, levels(captionData)[which] <- newCaption %||% levels(captionData)[which])
  return(captionData)
}
