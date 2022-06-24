#' @title LabelConfiguration
#' @description R6 class defining the configuration of the labels of a `ggplot` object
#' @export
LabelConfiguration <- R6::R6Class(
  "LabelConfiguration",
  public = list(
    #' @description Create a new `LabelConfiguration` object
    #' @param title character or `Label` object defining title
    #' @param subtitle character or `Label` object defining subtitle
    #' @param xlabel character or `Label` object defining xlabel
    #' @param ylabel character or `Label` object defining ylabel
    #' @param caption character or `Label` object defining caption
    #' @return A new `LabelConfiguration` object
    initialize = function(title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL,
                          caption = NULL) {
      inputs <- c("title", "subtitle", "xlabel", "ylabel", "caption")
      validateExpressions <- parse(text = paste0("validateIsOfType(", inputs, ', c("Label", "character"), nullAllowed =TRUE)'))
      eval(validateExpressions)

      currentTheme <- tlfEnv$currentTheme$clone(deep = TRUE)
      enforceLabelExpressions <- parse(text = paste0(
        "if(!isOfType(", inputs, ',"Label")){',
        inputs, "<- asLabel(text = ", inputs, ", font = currentTheme$fonts$", inputs, ")}"
      ))
      eval(enforceLabelExpressions)

      associateExpressions <- parse(text = paste0("private$.", inputs, " <- asLabel(", inputs, ")"))
      eval(associateExpressions)
    },

    #' @description Update labels of a `ggplot` object and their properties
    #' @param plotObject a `ggplot` object
    #' @return A `ggplot` object
    updatePlot = function(plotObject) {
      validateIsOfType(plotObject, "ggplot")
      # Update titles and axes labels
      plotObject <- plotObject + ggplot2::labs(
        title = private$.title$text,
        subtitle = private$.subtitle$text,
        x = private$.xlabel$text,
        y = private$.ylabel$text,
        caption = private$.caption$text
      )
      plotObject <- plotObject + ggplot2::theme(
        plot.title = private$.title$createPlotFont(),
        plot.subtitle = private$.subtitle$createPlotFont(),
        axis.title.x = private$.xlabel$createPlotFont(),
        axis.title.y = private$.ylabel$createPlotFont(),
        plot.caption = private$.caption$createPlotFont()
      )
      return(plotObject)
    }
  ),
  active = list(
    #' @field title `Label` object defining the title of the plot
    title = function(value) {
      if (missing(value)) {
        return(private$.title)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      if (isOfType(value, "Label")) {
        private$.title <- asLabel(value)
      }
      private$.title <- asLabel(value, font = private$.title$font)
      return(invisible())
    },
    #' @field subtitle `Label` object defining the subtitle of the plot
    subtitle = function(value) {
      if (missing(value)) {
        return(private$.subtitle)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      if (isOfType(value, "Label")) {
        private$.subtitle <- asLabel(value)
      }
      private$.subtitle <- asLabel(value, font = private$.subtitle$font)
      return(invisible())
    },
    #' @field xlabel `Label` object defining the xlabel of the plot
    xlabel = function(value) {
      if (missing(value)) {
        return(private$.xlabel)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      if (isOfType(value, "Label")) {
        private$.xlabel <- asLabel(value)
      }
      private$.xlabel <- asLabel(value, font = private$.xlabel$font)
      return(invisible())
    },
    #' @field ylabel `Label` object defining the ylabel of the plot
    ylabel = function(value) {
      if (missing(value)) {
        return(private$.ylabel)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      if (isOfType(value, "Label")) {
        private$.ylabel <- asLabel(value)
      }
      private$.ylabel <- asLabel(value, font = private$.ylabel$font)
      return(invisible())
    },
    #' @field caption `Label` object defining the caption of the plot
    caption = function(value) {
      if (missing(value)) {
        return(private$.caption)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      if (isOfType(value, "Label")) {
        private$.caption <- asLabel(value)
      }
      private$.caption <- asLabel(value, font = private$.caption$font)
      return(invisible())
    }
  ),
  private = list(
    .title = NULL,
    .subtitle = NULL,
    .xlabel = NULL,
    .ylabel = NULL,
    .caption = NULL
  )
)
