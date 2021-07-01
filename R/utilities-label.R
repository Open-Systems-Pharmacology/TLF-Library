#' @title setPlotLabels
#' @description Set labels properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param title character or `Label` object
#' @param subtitle character or `Label` object
#' @param xlabel character or `Label` object
#' @param ylabel character or `Label` object
#' @return ggplot object
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
    "if(isOfType(", inputs, ', "character")){',
    inputs, " <- asLabel(", inputs, ", font = labels$", inputs, "$font)}"
  ))
  eval(char2LabExpressions)
  eval(parseVariableToObject("labels", inputs, keepIfNull = TRUE))
  newPlotObject <- labels$updatePlot(newPlotObject)
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
