#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  ## ----------------------------------
  ## List of plotConfiguration Variables
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    watermark = NULL,
    colorLegendCaption = NULL,
    sizeLegendCaption = NULL,
    shapeLegendCaption = NULL,
    legendPosition = NULL,
    filename = NULL,

    ## ----------------------------------------------
    ## Initializing function to be called with $new()
    initialize = function(title = NULL,
                          subtitle = asLabel(paste("Date:", format(Sys.Date(), "%y-%m-%d"))),
                          xlabel = NULL,
                          ylabel = NULL,
                          xlim = c(-Inf, Inf),
                          ylim = c(-Inf, Inf),
                          watermark = "TLF-Watermark",
                          filename = "TestPlot.png",
                          colorLegendCaption = "",
                          sizeLegendCaption = "",
                          shapeLegendCaption = "",
                          legendPosition = "outsideRight",
                          data = NULL,
                          metaData = NULL,
                          dataMapping = NULL,
                          theme = tlfEnv$currentTheme) {
      self$title <- asLabel(title %||% "")
      self$title$font <- theme$title
      self$subtitle <- asLabel(subtitle %||% "")
      self$subtitle$font <- theme$subtitle

      # If xlabel and ylabel are not defined, use dataMapping of x, y to label axes
      xMapping <- NULL
      yMapping <- NULL
      if (!is.null(dataMapping)) {
        xMapping <- dataMapping$x
        yMapping <- dataMapping$y
      }
      self$xlabel <- asLabel(xlabel %||% (dataMappingLabel(xMapping, metaData) %||% ""))
      self$ylabel <- asLabel(ylabel %||% (dataMappingLabel(yMapping, metaData) %||% ""))

      self$xlabel$font <- theme$xlabel
      self$ylabel$font <- theme$ylabel

      self$watermark <- asLabel(watermark %||% "")
      self$watermark$font <- theme$watermark

      self$filename <- filename
      self$colorLegendCaption <- colorLegendCaption
      self$sizeLegendCaption <- sizeLegendCaption
      self$shapeLegendCaption <- shapeLegendCaption
      self$legendPosition <- legendPosition
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
    ## Define Labels: plotConfiguration function that uses data mapping
    ## to set labels and legends
    setPlotLabels = function(plotHandle, dataMapping) {

      # Titles and axes labels
      plotHandle <- plotHandle + ggplot2::labs(
        title = self$title$text,
        subtitle = self$subtitle$text,
        x = self$xlabel$text,
        y = self$ylabel$text
      )

      plotHandle <- setFontProperties(
        plotHandle,
        titleFont = self$title$font,
        subtitleFont = self$subtitle$font,
        xAxisFont = self$xlabel$font,
        yAxisFont = self$ylabel$font,
        legendFont = self$legendCaption$font
      )

      # Legend labels
      plotHandle <- setLegendPosition(plotHandle, legendPosition = self$legendPosition)

      # Redefine label of groups in legend
      plotHandle <- plotHandle + ifnotnull(
        dataMapping$colorGrouping,
        ggplot2::guides(color = guide_legend(paste(dataMapping$colorGrouping, collapse = "-"))),
        ggplot2::guides(color = "none")
      )

      plotHandle <- plotHandle + ifnotnull(
        dataMapping$sizeGrouping,
        ggplot2::guides(size = guide_legend(paste(dataMapping$sizeGrouping, collapse = "-"))),
        ggplot2::guides(size = "none")
      )

      plotHandle <- plotHandle + ifnotnull(
        dataMapping$shapeGrouping,
        ggplot2::guides(shape = guide_legend(paste(dataMapping$shapeGrouping, collapse = "-"))),
        ggplot2::guides(shape = "none")
      )

      return(plotHandle)
    },

    ## ----------------------------------------------------------
    ## Legend Captions: plotConfiguration function to relabel and re-order legendcaption
    relabelColorCaption = function(data) {
      data$colorGrouping <- updateLegendCaption(captionData = data$colorGrouping, newCaption = colorLegendCaption)
      return(data)
    },

    relabelSizeCaption = function(data) {
      data$sizeGrouping <- updateLegendCaption(captionData = data$sizeGrouping, newCaption = sizeLegendCaption)
      return(data)
    },

    relabelShapeCaption = function(data) {
      data$shapeGrouping <- updateLegendCaption(captionData = data$shapeGrouping, newCaption = shapeLegendCaption)
      return(data)
    },

    ## ----------------------------------------------------------
    ## Define Labels: plotConfiguration function to first define Watermark

    setWatermark = function(plotHandle) {
      plotHandle <- setWatermark(plotHandle = plotHandle, label = self$watermark)
      return(plotHandle)
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
  ifnotnull(mapping, label <- getLabelWithUnit(mapping, metaData[[mapping]]$Unit))

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
