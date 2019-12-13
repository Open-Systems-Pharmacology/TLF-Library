#' @title PlotConfiguration
#' @docType class
#' @description  Class for Plot Configuration
#' @field legend \code{LegendConfiguration} R6 class defining legend properties
#' @field xAxis \code{XAxisConfiguration} R6 class defining X-axis properties
#' @field yAxis \code{YAxisConfiguration} R6 class defining Y-axis properties
#' @field background \code{BackgroundConfiguration} R6 class defining background properties
#' @field saveConfiguration  \code{SaveConfiguration} R6 class defining save properties
#' @field theme \code{Theme} R6 class defining theme aesthetic properties
#' @section Methods:
#' \describe{
#' \item{new(...)}{Initialize PlotConfiguration}
#' \item{setPlotProperties(plotObject)}{Apply plot labels properties to plotObject}
#' \item{setPlotBackground(plotObject)}{Apply background properties to plotObject}
#' \item{savePlot(plotObject)}{Save plotObject into a file.}
#' }
#' @export
#' @format NULL
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  inherit = LabelConfiguration,
  ## ----------------------------------
  ## List of plotConfiguration Variables
  public = list(
    legend = NULL,
    xAxis = NULL,
    yAxis = NULL,
    background = NULL,
    saveConfiguration = NULL,
    theme = NULL,
    
    ## ----------------------------------------------
    ## Initializing function to be called with $new()
    initialize = function(
      # Label configuration
      title = NULL,
      subtitle = NULL,
      xlabel = NULL,
      ylabel = NULL,
      # Legend Configuration
      legend = NULL,
      legendTitles = NULL,
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
      # Save configuration
      saveConfiguration = NULL,
      filename = NULL,
      width = NULL,
      height = NULL,
      units = NULL,
      # Smart configuration using metaData
      data = NULL,
      metaData = NULL,
      dataMapping = NULL,
      # Theme
      theme = tlfEnv$currentTheme, ...) {
      
      super$initialize(
        title = title,
        subtitle = subtitle,
        xlabel = xlabel,
        ylabel = ylabel
      )

      # Smart configuration if xlabel and ylabel are not defined, 
      # use dataMapping of x, y to label axes
      xMapping <- NULL
      yMapping <- NULL
      if (!is.null(data) && is.null(dataMapping)) {
        dataMapping <- XYDataMapping$new(data = data)
      }
      if (!is.null(dataMapping)) {
        xMapping <- dataMapping$x
        yMapping <- dataMapping$y
      }
      self$xlabel <- asLabel(xlabel %||% (dataMappingLabel(xMapping, metaData) %||% ""))
      self$ylabel <- asLabel(ylabel %||% (dataMappingLabel(yMapping, metaData) %||% ""))

      self$xlabel$font <- theme$xlabelFont
      self$ylabel$font <- theme$ylabelFont

      # Smart configuration if legend is not defined, 
      self$legend <- legend %||% ifnotnull(
        dataMapping,
        LegendConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping),
        LegendConfiguration$new()
      )
      
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
      
      # Define save configuration, overwrite properties only if they are defined
      self$saveConfiguration <- saveConfiguration %||% SaveConfiguration$new()
      self$saveConfiguration$filename <- filename %||% self$saveConfiguration$filename
      self$saveConfiguration$width <- width %||% self$saveConfiguration$width
      self$saveConfiguration$height <- height %||% self$saveConfiguration$height
      self$saveConfiguration$units <- units %||% self$saveConfiguration$units
      
      self$theme <- theme
      
    },
    
    ## ---------------------------------------------------------------
    ## Define Labels: plotConfiguration function to first define Watermark

    setPlotProperties = function(plotObject) {
      # plotObject <- self$legend$setPlotLegend(plotObject)

      plotObject <- self$xAxis$setPlotAxis(plotObject)
      plotObject <- self$yAxis$setPlotAxis(plotObject)

      return(plotObject)
    },

    setPlotBackground = function(plotObject) {
      plotObject <- self$background$setBackground(plotObject)
      return(plotObject)
    },

    savePlot = function(plotObject) {
      self$saveConfiguration$savePlot(plotObject)
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
#' @export
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
