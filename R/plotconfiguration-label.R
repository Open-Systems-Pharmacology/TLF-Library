#' @title LabelConfiguration
#' @description R6 class defining the configuration of the labels of a \code{ggplot} object
#' @export
LabelConfiguration <- R6::R6Class(
  "LabelConfiguration",
  public = list(
    #' @field title R6 class \code{Label} object
    title = NULL,
    #' @field subtitle R6 class \code{Label} object
    subtitle = NULL,
    #' @field xlabel R6 class \code{Label} object
    xlabel = NULL,
    #' @field ylabel R6 class \code{Label} object
    ylabel = NULL,
    #' @field legendTitles List of legend titles
    legendTitles = NULL,

    #' @description Create a new \code{LabelConfiguration} object
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param legendTitles List of legend titles
    #' @param theme R6 class \code{Theme}
    #' @return A new \code{LabelConfiguration} object
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

    #' @description Set plot labels properties of a \code{ggplot} object
    #' @param plotObject a \code{ggplot} object
    #' @return A \code{ggplot} object
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
