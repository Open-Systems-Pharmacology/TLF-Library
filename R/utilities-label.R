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

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  labels <- newPlotObject$plotConfiguration$labels

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

#' @title asLabel
#' @param text text to be set as label class
#' @param font font properties of Label
#' @return Label class object
#' @description
#' Set a character string into a Label class associating font properties to input
#' If text is already a Label class, asLabel can be used to updated the font properties
#' @export
#' @examples
#' title <- "Title of Plot"
#' title <- asLabel(title)
asLabel <- function(text = "", font = NULL) {
  # Check input
  validateIsOfType(text, c("Label", "character"), nullAllowed = TRUE)
  validateIsOfType(font, Font, nullAllowed = TRUE)

  # Ensure Label class
  if (!isOfType(text, "Label")) {
    text <- Label$new(text)
  }

  # If font is not null, set font properties of Label
  text$font <- font %||% text$font

  return(text)
}

#' @title setFontProperties
#' @param plotObject ggplot object
#' @param titleFont Font Class for title
#' @param subtitleFont Font Class for subtitle
#' @param xAxisFont Font Class for xaxis and ticks
#' @param yAxisFont Font Class for yaxis and ticks
#' @param legendFont Font Class for legend
#' @return plotObject ggplot object with updated fonts
#' @description
#' setFontProperties set Font Properties on a ggplot object
#' @include font.R
#' @export
#' @examples
#' p <- ggplot2::ggplot() + ggplot2::labs(title = "Title")
#' newFont <- Font$new(color = "blue", size = 20)
#' p <- setFontProperties(plotObject = p, titleFont = newFont)
setFontProperties <- function(plotObject,
                              titleFont = NULL,
                              subtitleFont = NULL,
                              xAxisFont = NULL,
                              yAxisFont = NULL,
                              legendFont = NULL) {
  if (!is.null(titleFont)) {
    plotObject <- plotObject + theme(plot.title = titleFont$setFont())
  }
  if (!is.null(subtitleFont)) {
    plotObject <- plotObject + theme(plot.subtitle = subtitleFont$setFont())
  }
  if (!is.null(xAxisFont)) {
    plotObject <- plotObject + theme(axis.title.x = xAxisFont$setFont(), axis.text.x = xAxisFont$setFont())
  }
  if (!is.null(yAxisFont)) {
    plotObject <- plotObject + theme(axis.title.y = yAxisFont$setFont(), axis.text.y = yAxisFont$setFont())
  }
  if (!is.null(legendFont)) {
    plotObject <- plotObject + theme(legend.text = legendFont$setFont())
  }

  return(plotObject)
}
