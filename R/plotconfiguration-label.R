#' @title LabelConfiguration
#' @description R6 class defining the configuration of the labels of a \code{ggplot} object
#' @export
LabelConfiguration <- R6::R6Class(
  "LabelConfiguration",
  public = list(
    #' @description Create a new \code{LabelConfiguration} object
    #' @param title character or \code{Label} object defining title
    #' @param subtitle character or \code{Label} object defining subtitle
    #' @param xlabel character or \code{Label} object defining xlabel
    #' @param ylabel character or \code{Label} object defining ylabel
    #' @return A new \code{LabelConfiguration} object
    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL) {
      inputs <- c("title", "subtitle", "xlabel", "ylabel")
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

    #' @description Update labels of a \code{ggplot} object and their properties
    #' @param plotObject a \code{ggplot} object
    #' @return A \code{ggplot} object
    updatePlot = function(plotObject) {
      validateIsOfType(plotObject, "ggplot")
      # Update titles and axes labels
      plotObject <- plotObject + ggplot2::labs(
        title = private$.title$text,
        subtitle = private$.subtitle$text,
        x = private$.xlabel$text,
        y = private$.ylabel$text
      )
      plotObject <- plotObject + ggplot2::theme(
        plot.title = private$.title$createPlotFont(),
        plot.subtitle = private$.subtitle$createPlotFont(),
        axis.title.x = private$.xlabel$createPlotFont(),
        axis.title.y = private$.ylabel$createPlotFont()
      )
      return(plotObject)
    }
  ),
  active = list(
    #' @field title \code{Label} object defining the title of the plot
    title = function(value) {
      if (missing(value)) {
        return(private$.title)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      private$.title <- asLabel(value)
      return(invisible())
    },
    #' @field subtitle \code{Label} object defining the subtitle of the plot
    subtitle = function(value) {
      if (missing(value)) {
        return(private$.subtitle)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      private$.subtitle <- asLabel(value)
      return(invisible())
    },
    #' @field xlabel \code{Label} object defining the xlabel of the plot
    xlabel = function(value) {
      if (missing(value)) {
        return(private$.xlabel)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      private$.xlabel <- asLabel(value)
      return(invisible())
    },
    #' @field ylabel \code{Label} object defining the ylabel of the plot
    ylabel = function(value) {
      if (missing(value)) {
        return(private$.ylabel)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      private$.ylabel <- asLabel(value)
      return(invisible())
    }
  ),
  private = list(
    .title = NULL,
    .subtitle = NULL,
    .xlabel = NULL,
    .ylabel = NULL
  )
)
