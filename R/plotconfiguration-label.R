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

    #' @description Create a new \code{LabelConfiguration} object
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param theme R6 class \code{Theme}
    #' @return A new \code{LabelConfiguration} object
    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL,
                              theme = tlfEnv$currentTheme) {
      inputs <- c("title", "subtitle", "xlabel", "ylabel")
      validateExpressions <- parse(text = paste0("validateIsOfType(", inputs, ', c("Label", "character"), nullAllowed =TRUE)'))
      eval(validateExpressions)

      enforceLabelExpressions <- parse(text = paste0(
        "if(isOfType(", inputs, ',"character")){',
        inputs, "<- asLabel(text = ", inputs, ", font = theme$", inputs, "Font)}"
      ))
      eval(enforceLabelExpressions)

      associateExpressions <- parse(text = paste0("self$", inputs, " <- asLabel(", inputs, ")"))
      eval(associateExpressions)
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
        yAxisFont = self$ylabel$font
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
            Value = self[[elementName]]$text %||% "NULL"
          )
        )
      }
      return(labelProperties)
    }
  )
)
