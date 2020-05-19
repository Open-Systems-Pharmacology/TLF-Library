#' @title PlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  public = list(
    #' @field labels R6 class \code{LabelConfiguration} defining labels properties
    labels = NULL,
    #' @field legend R6 class \code{LegendConfiguration} defining legend properties
    legend = NULL,
    #' @field xAxis R6 class \code{XAxisConfiguration} defining X-axis properties
    xAxis = NULL,
    #' @field yAxis R6 class \code{YAxisConfiguration} defining Y-axis properties
    yAxis = NULL,
    #' @field background R6 class \code{BackgroundConfiguration} defining background properties
    background = NULL,
    #' @field export R6 class \code{ExportConfiguration} defining export properties
    export = NULL,
    #' @field theme \code{Theme} R6 class defining theme aesthetic properties
    theme = NULL,

    #' @description Create a new \code{PlotConfiguration} object
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param legend R6 class \code{LegendConfiguration} object defining legend properties
    #' @param legendTitle character legend title
    #' @param legendPosition character legend position. 
    #' Use Enum `LegendPositions` to get a list of available to legend positions.
    #' @param xAxis R6 class \code{XAxisConfiguration} object defining X-axis properties
    #' @param xScale character defining X-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param xLimits numeric vector of X-axis limits
    #' @param yAxis R6 class \code{YAxisConfiguration} object defining X-axis properties
    #' @param yScale character defining Y-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param yLimits numeric vector of Y-axis limits
    #' @param background R6 class \code{BackgroundConfiguration} defining background properties
    #' @param watermark R6 class \code{Label} object defining watermark background
    #' @param export R6 class \code{SaveConfiguration} defining saving properties
    #' @param format character defining the format of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param data data.frame used by \code{smartMapping}
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class or subclass \code{XYDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @return A new \code{PlotConfiguration} object
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
                              dataMapping = NULL,
                              # Theme
                              theme = tlfEnv$currentTheme) {
      
      self$labels<-LabelConfiguration$new(title=title,subtitle=subtitle,
                                          xlabel=xlabel,ylabel=ylabel,
                                          theme=theme)

      # Smart configuration if xlabel and ylabel
      # 1) If xlabel and ylabel provided: use as is.
      # 2) Else, if data, metaData (and optionally dataMapping) provided: use dimension [unit] from metaData
      # 3) Else, if data (and optionally dataMapping) provided: use variable names from data
      if (!is.null(data)) {
        dataMapping <- dataMapping %||% XYGDataMapping$new(data = data)
      }
      self$labels$xlabel <- asLabel(xlabel %||%
        dataMappingLabel(dataMapping$x, metaData) %||%
        dataMapping$x %||%
        self$labels$xlabel)
      self$labels$ylabel <- asLabel(ylabel %||%
        dataMappingLabel(dataMapping$y, metaData) %||%
        dataMapping$y %||%
        dataMappingLabel(dataMapping$ymax, metaData) %||%
        dataMapping$ymax %||%
        self$labels$ylabel)

      # Smart configuration if legend is not defined,
      self$legend <- legend %||% LegendConfiguration$new()
      self$legend$title <- legendTitle %||% self$legend$title
      validateIsIncluded(legendPosition, "LegendPositions", nullAllowed = TRUE)
      self$legend$position <- legendPosition %||% self$legend$position

      # Define X-Axis configuration, overwrite properties only if they are defined
      self$xAxis <- xAxis %||% XAxisConfiguration$new()
      self$xAxis$limits <- xLimits %||% self$xAxis$limits
      self$xAxis$scale <- xScale %||% self$xAxis$scale

      # Define Y-Axis configuration, overwrite properties only if they are defined
      self$yAxis <- yAxis %||% YAxisConfiguration$new()
      self$yAxis$limits <- yLimits %||% self$xAxis$limits
      self$yAxis$scale <- yScale %||% self$xAxis$scale

      # Set background properties
      self$background <- background %||% BackgroundConfiguration$new(
        watermark = watermark,
        theme = theme
      )
      self$background$watermark <- watermark %||% self$background$watermark

      # Define export configuration, overwrite properties only if they are defined
      self$export <- export %||% ExportConfiguration$new()
      self$export$format <- format %||% self$export$format
      self$export$width <- width %||% self$export$width
      self$export$height <- height %||% self$export$height
      self$export$units <- units %||% self$export$units

      self$theme <- theme
    },

    #' @description Print plot configuration
    #' @return Plot configuration
    print = function() {
      plotProperties <- list(
        labels = self$labels$print(),
        legend = self$legend$print(),
        xAxis = self$xAxis$print(),
        yAxis = self$yAxis$print(),
        background = self$background$print(),
        saveConfiguration = self$saveConfiguration$print()
      )
      return(plotProperties)
    }
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


setAesPropertiesToLegend <- function(parameter = NULL, aesProperties = NULL) {
  ifnotnull(
    aesProperties,
    stopifnot(aesProperties %in% c("color", "colour", "size", "shape", "linetype")),
    return(NULL)
  )

  if (!is.null(parameter)) {
    if (is.list(parameter)) {
      if (min(names(parameter) %in% aesProperties) == 1) {
        names(parameter) <- aesProperties
      }
    }
    # Case where legends are provided as characters only
    if (is.character(parameter)) {
      parameter2 <- parameter
      parameter <- enum(aesProperties)
      for (property in aesProperties) {
        parameter[[property]] <- parameter2
      }
    }
  }
  return(parameter)
}
