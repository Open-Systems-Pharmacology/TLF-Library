#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  inherit = LabelConfiguration,
  ## ----------------------------------
  ## List of plotConfiguration Variables
  public = list(
    legend = NULL, # R6 class
    xAxis = NULL, # R6 class
    yAxis = NULL, # R6 class

    colorMap = NULL,
    sizeMap = NULL,
    shapeMap = NULL,

    filename = NULL,

    ## ----------------------------------------------
    ## Initializing function to be called with $new()
    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = tlfEnv$currentTheme$watermarkText,
                              legendTitles = NULL,
                              filename = "TestPlot.png",
                              legend = NULL, # R6 class
                              xAxis = NULL,
                              yAxis = NULL,
                              data = NULL,
                              metaData = NULL,
                              dataMapping = NULL,
                              theme = tlfEnv$currentTheme, ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        xlabel = xlabel,
        ylabel = ylabel,
        watermark = watermark
      )

      # If xlabel and ylabel are not defined, use dataMapping of x, y to label axes
      xMapping <- NULL
      yMapping <- NULL
      if (!is.null(dataMapping)) {
        xMapping <- dataMapping$x
        yMapping <- dataMapping$y
      }
      self$xlabel <- asLabel(xlabel %||% (dataMappingLabel(xMapping, metaData) %||% ""))
      self$ylabel <- asLabel(ylabel %||% (dataMappingLabel(yMapping, metaData) %||% ""))

      self$xlabel$font <- theme$xlabelFont
      self$ylabel$font <- theme$ylabelFont



      self$filename <- filename

      self$legend <- legend %||% ifnotnull(
        dataMapping,
        LegendConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping),
        LegendConfiguration$new()
      )

      self$xAxis <- xAxis %||% XAxisConfiguration$new()
      self$yAxis <- yAxis %||% YAxisConfiguration$new()
    },

    ## ---------------------------------------------------------------
    ## Printing function, only show main elements of plotConfiguration

    print = function() {
      cat("  Title: ", self$title$text, "\n", sep = "\t")
      cat("  Subtitle: ", self$subtitle$text, "\n", sep = "\t")
      cat("  Xlabel:  ", self$xlabel$text, "\n", sep = "\t")
      cat("  Ylabel:  ", self$ylabel$text, "\n", sep = "\t")
      invisible(self)
    },

    ## ---------------------------------------------------------------
    ## Define Labels: plotConfiguration function to first define Watermark

    setPlotProperties = function(plotObject) {
      # plotObject <- self$legend$setPlotLegend(plotObject)

      plotObject <- self$xAxis$setPlotAxis(plotObject)
      plotObject <- self$yAxis$setPlotAxis(plotObject)

      return(plotObject)
    },

    savePlot = function(plotHandle) {
      ggplot2::ggsave(filename = self$filename, plotHandle, width = 20, height = 12, units = "cm")
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
