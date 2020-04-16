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
    },

    #' @description Print plot label properties
    #' @return Plot label properties
    print = function() {
      labelProperties <- NULL

      # Get properties that are of Label type
      elementProperties <- unlist(eapply(
        self,
        function(x) {
          isOfType(x, "Label")
        }
      ))
      elementNames <- names(elementProperties[as.logical(elementProperties)])

      # Build data.frame of properties while removing NULL values
      for (elementName in elementNames) {
        labelProperties <- rbind.data.frame(
          labelProperties,
          data.frame(
            Property = elementName,
            Value = self[[elementName]]$text
          )
        )
      }
      return(labelProperties)
    }
  )
)

#' @title setPlotLabels
#' @description Set labels properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param title character or Label class object
#' @param subtitle character or Label class object
#' @param xlabel character or Label class object
#' @param ylabel character or Label class object
#' @return ggplot object with updated labels
#' @export
setPlotLabels <- function(plotObject,
                          title = NULL,
                          subtitle = NULL,
                          xlabel = NULL,
                          ylabel = NULL) {
  validateIsOfType(plotObject, "ggplot")

  # Inputs will undergo the same code, so parse/eval
  # parse/eval of inputs prevent copy paste of code
  inputs <- c("title", "subtitle", "xlabel", "ylabel")
  validateExpressions <- parse(text = paste0("validateIsOfType(", inputs, ', c("Label", "character"), nullAllowed =TRUE)'))
  eval(validateExpressions)

  # Clone tlfConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$tlfConfiguration <- plotObject$tlfConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$tlfConfiguration$yAxis
  labels <- newPlotObject$tlfConfiguration$labels

  char2LabExpressions <- parse(text = paste0(
    "if(!is.null(", inputs, ")){",
    "if(isOfType(", inputs, ', "character")){',
    inputs, " <- asLabel(", inputs, ", labels$", inputs, "$font)}}"
  ))
  eval(char2LabExpressions)

  updateLabelExpressions <- parse(text = paste0("labels$", inputs, " <- ", inputs, " %||% labels$", inputs))
  eval(updateLabelExpressions)

  newPlotObject <- labels$setPlotLabels(newPlotObject)

  return(newPlotObject)
}
