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
                              watermark = tlfEnv$currentTheme$watermarkText,
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
