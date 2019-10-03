#' @title LabelConfiguration
#' @docType class
#' @description  Generic Label Configuration
#' @export
LabelConfiguration <- R6::R6Class(
  "LabelConfiguration",
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    watermark = NULL,
    legendTitles = NULL,

    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = NULL,
                              legendTitles = NULL,
                              theme = tlfEnv$currentTheme) {
      self$title <- asLabel(title %||% "")
      self$title$font <- theme$titleFont
      self$subtitle <- asLabel(subtitle %||% "")
      self$subtitle$font <- theme$subtitleFont

      self$xlabel <- asLabel(xlabel %||% "")
      self$xlabel$font <- theme$xlabelFont
      self$ylabel <- asLabel(ylabel %||% "")
      self$ylabel$font <- theme$ylabelFont

      self$watermark <- asLabel(watermark %||% "")
      self$watermark$font <- theme$watermarkFont

      self$legendTitles <- asLabel(legendTitles %||% "")
      self$legendTitles$font <- theme$legendTitles
    },

    ## ---------------------------------------------------------------
    ## Define Labels: plotConfiguration function that uses data mapping
    ## to set labels and legends
    setPlotLabels = function(plotObject) {

      # Titles and axes labels
      plotObject <- plotObject + ggplot2::labs(
        title = self$title$text,
        subtitle = self$subtitle$text,
        x = self$xlabel$text,
        y = self$ylabel$text
      )

      plotObject <- setFontProperties(
        plotObject,
        titleFont = self$title$font,
        subtitleFont = self$subtitle$font,
        xAxisFont = self$xlabel$font,
        yAxisFont = self$ylabel$font,
        legendFont = self$legendTitles$font
      )
      return(plotObject)
    },

    setWatermark = function(plotObject) {
      plotObject <- setWatermark(plotHandle = plotObject, label = self$watermark)
      return(plotObject)
    }
  )
)

#' @title LegendConfiguration
#' @docType class
#' @description  Generic Legend Configuration
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    position = NULL,
    aesProperties = NULL,
    titles = NULL,
    captions = NULL,
    values = NULL,

    initialize = function(position = NULL,
                              aesProperties = "color",
                              titles = NULL,
                              captions = NULL,
                              values = NULL,
                              dataMapping = NULL,
                              data = NULL,
                              metaData = NULL,
                              theme = tlfEnv$currentTheme) {
      ifnotnull(
        position,
        self$position <- position,
        self$position <- legendPositions$outsideRight
      )

      if (!is.null(dataMapping)) {
        mapData <- dataMapping$getMapData(data, metaData)
        mapTitles <- list()
        mapCaptions <- list()
        for (aesProperty in aesProperties) {
          mapTitles[[aesProperty]] <- dataMapping[[aesProperty]]$groupingLegendTitle %||% paste(dataMapping[[aesProperty]]$groupingName,collapse="-")  #for second option, what if groupingName is a vector of strings?
          mapCaptions[[aesProperty]] <- mapData[[aesProperty]]
        }
      }
      self$titles <- ifnotnull(
        titles,
        setAesPropertiesToLegend(titles, aesProperties),
        mapTitles
      )
      self$captions <- ifnotnull(
        captions,
        setAesPropertiesToLegend(captions, aesProperties),
        mapCaptions
      )

      self$aesProperties <- aesProperties

      newValues <- values %||% theme$aesProperties
      self$values <- setAesPropertiesToLegend(newValues, aesProperties)
    },

    setPlotLegend = function(plotObject) {
      plotObject <- setLegendPosition(plotObject, legendPosition = self$position)

      for (aesProperty in self$aesProperties) {
        plotObject <- plotObject +
          ifnotnull(
            self$captions[[aesProperty]],

            scale_discrete_manual(
              aesthetics = aesProperty,
              name = self$titles[[aesProperty]],
              values = self$values[[aesProperty]],
              labels = self$captions[[aesProperty]]
            ),

            scale_discrete_manual(
              aesthetics = aesProperty,
              name = self$titles[[aesProperty]],
              values = self$values[[aesProperty]],
              labels = NA,
              guide = "none"
            )
          )
      }
      return(plotObject)
    }
  )
)

#' @title AxisConfiguration
#' @docType class
#' @description  Generic axis Configuration
#' @export
AxisConfiguration <- R6::R6Class(
  "AxisConfiguration",
  public = list(
    limits = NULL,
    scale = NULL,
    ticks = NULL,
    ticklabels = NULL,

    initialize = function(limits = NULL,
                              scale = "lin",
                              ticks = "default",
                              ticklabels = "default") {
      self$limits <- limits
      self$scale <- scale
      if (self$scale %in% "lin") {
        self$scale <- "identity"
      }

      self$ticks <- ticks
      if (ticks %in% "default") {
        self$ticks <- waiver()
      }
      self$ticklabels <- ticklabels
      if (ticklabels %in% "default") {
        self$ticklabels <- waiver()
      }
    }
  )
)

#' @title XAxisConfiguration
#' @docType class
#' @description  Generic X axis Configuration
#' @export
XAxisConfiguration <- R6::R6Class(
  "XAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_x_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
    }
  )
)

#' @title YAxisConfiguration
#' @docType class
#' @description  Generic Y axis Configuration
#' @export
YAxisConfiguration <- R6::R6Class(
  "YAxisConfiguration",
  inherit = AxisConfiguration,
  public = list(
    setPlotAxis = function(plotObject) {
      plotObject <- plotObject +
        scale_y_continuous(trans = self$scale, limits = self$limits, breaks = self$ticks, labels = self$ticklabels)
    }
  )
)

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
                              watermark = NULL,
                              legendTitles = NULL,
                              filename = "TestPlot.png",
                              legend = NULL, # R6 class
                              xAxis = NULL,
                              yAxis = NULL,
                              data = NULL,
                              metaData = NULL,
                              dataMapping = NULL,
                              theme = tlfEnv$currentTheme, ...) {
      super$initialize(...)

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
      plotObject <- self$legend$setPlotLegend(plotObject)

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
