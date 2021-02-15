#' @title LegendConfiguration
#' @description R6 class defining the legend configuration of a \code{ggplot} object
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    #' @description Create a new \code{LegendConfiguration} object
    #' @param position position of the legend as defined by enum `LegendPositions`
    #' @param caption data.frame containing the properties of the legend caption
    #' @param title character title of the legend caption. A value of `NULL` removes the title.
    #' @param titleFont \code{Font} object defining the font of the legend title
    #' @param font \code{Font} object defining the font of the legend caption
    #' @param background \code{BackgroundElement} object defining the background of the legend
    #' @return A new \code{LegendConfiguration} object
    initialize = function(position = tlfEnv$currentTheme$background$legendPosition,
                              caption = NULL,
                              title = NULL,
                              titleFont = NULL,
                              font = NULL,
                              background = NULL) {
      validateIsIncluded(position, LegendPositions)
      validateIsString(title, nullAllowed = TRUE)
      validateIsOfType(titleFont, "Font", nullAllowed = TRUE)
      validateIsOfType(font, "Font", nullAllowed = TRUE)
      validateIsOfType(background, "BackgroundElement", nullAllowed = TRUE)

      private$.position <- position
      private$.font <- font %||% tlfEnv$currentTheme$fonts$legend
      private$.titleFont <- titleFont %||% tlfEnv$currentTheme$fonts$legendTitle
      private$.background <- background %||% tlfEnv$currentTheme$background$legend

      private$.title <- title
      private$.caption <- caption %||% data.frame()
    },

    #' @description Update legend configuration on a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object with updated axis properties
    updatePlot = function(plotObject) {
      validateIsOfType(plotObject, "ggplot")
      # Update legend background, font and title font
      plotObject <- plotObject +
        ggplot2::theme(
          legend.background = private$.background$createPlotElement(),
          legend.text = private$.font$createPlotFont(),
          legend.title = private$.titleFont$createPlotFont()
        )

      # For legend title, if no title, element_blank should be used
      if (isOfLength(private$.title, 0)) {
        plotObject <- plotObject + ggplot2::theme(legend.title = ggplot2::element_blank())
      }
      # Update legend position
      legendPosition <- getLegendPosition(private$.position)
      plotObject <- plotObject + ggplot2::theme(
        legend.position = c(legendPosition$xPosition, legendPosition$yPosition),
        legend.justification = c(legendPosition$xJustification, legendPosition$yJustification),
        legend.direction = "vertical"
      )
      # TO DO: update caption properties before returning plot
      return(plotObject)
    }
  ),
  active = list(
    #' @field caption of legend defined as data.frame with caption properties
    caption = function(value) {
      if (missing(value)) {
        return(private$.caption)
      }
      validateIsOfType(value, "data.frame")
      private$.caption <- value
      return(invisible())
    },
    #' @field position of legend as defined in Enum `LegendPositions`
    position = function(value) {
      if (missing(value)) {
        return(private$.position)
      }
      validateIsIncluded(value, LegendPositions)
      private$.position <- value
      return(invisible())
    },
    #' @field font \code{Font} object defining the font of the legend
    font = function(value) {
      if (missing(value)) {
        return(private$.font)
      }
      validateIsOfType(value, "Font", nullAllowed = TRUE)
      private$.font <- value %||% tlfEnv$currentTheme$fonts$legend
      return(invisible())
    },
    #' @field titleFont \code{Font} object defining the font of the legend title
    titleFont = function(value) {
      if (missing(value)) {
        return(private$.titleFont)
      }
      validateIsOfType(value, "Font", nullAllowed = TRUE)
      private$.titleFont <- value %||% tlfEnv$currentTheme$fonts$legendTitle
      return(invisible())
    },
    #' @field background \code{Background} object defining the background of the legend
    background = function(value) {
      if (missing(value)) {
        return(private$.background)
      }
      validateIsOfType(value, "BackgroundElement", nullAllowed = TRUE)
      private$.background <- value %||% tlfEnv$currentTheme$background$legend
      return(invisible())
    },
    #' @field title character defining title of the legend
    title = function(value) {
      if (missing(value)) {
        return(private$.title)
      }
      validateIsString(value, nullAllowed = TRUE)
      private$.title <- value
      return(invisible())
    }
  ),
  private = list(
    .position = NULL,
    .title = NULL,
    .titleFont = NULL,
    .font = NULL,
    .background = NULL,
    .caption = NULL
  )
)
