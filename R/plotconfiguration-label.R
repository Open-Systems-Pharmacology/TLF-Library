#' @title LabelConfiguration
#' @docType class
#' @description  Class for Label Configuration
#' @field title Label properties of title
#' @field subtitle Label properties of subtitle
#' @field xlabel Label properties of xlabel
#' @field ylabel Label properties of ylabel
#' @field legendTitles Label properties of legendTitles
#' @section Methods:
#' \describe{
#' \item{new(title = NULL, subtitle = NULL, xlabel = NULL, ylabel = NULL, legendTitles = NULL, theme = tlfEnv$currentTheme)}{
#' Initialize LabelConfiguration. Set font properties for each label of the plot.}
#' \item{setPlotLabels(plotObject)}{Apply properties of plot labels.}
#' }
#' @export
LabelConfiguration <- R6::R6Class(
  "LabelConfiguration",
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    legendTitles = NULL,

    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL,
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
    }
  )
)
