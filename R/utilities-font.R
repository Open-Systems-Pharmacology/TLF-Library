# Utils fonctions for Font and Label class objects

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
  validateIsOfType(text, c("Label", "character"))
  validateIsOfType(font, Font, nullAllowed = TRUE)

  # If text is already a Label class
  if (("character" %in% class(text))) {
    text <- Label$new(text)
  }

  # If font is not null, set font properties of Label
  text$font <- font %||% text$font

  return(text)
}

#' @title setFontProperties
#' @param plotHandle ggplot object
#' @param titleFont Font Class for title
#' @param subtitleFont Font Class for subtitle
#' @param xAxisFont Font Class for xaxis and ticks
#' @param yAxisFont Font Class for yaxis and ticks
#' @param legendFont Font Class for legend
#' @return plotHandle ggplot object with updated fonts
#' @description
#' setFontProperties set Font Properties on a ggplot object
#' @include font.R
#' @export
#' @examples
#' p <- ggplot2::ggplot() + ggplot2::labs(title = "Title")
#' newFont <- Font$new(color = "blue", size = 20)
#' p <- setFontProperties(plotHandle = p, titleFont = newFont)
setFontProperties <- function(plotHandle,
                              titleFont = NULL,
                              subtitleFont = NULL,
                              xAxisFont = NULL,
                              yAxisFont = NULL,
                              legendFont = NULL) {
  if (!is.null(titleFont)) {
    plotHandle <- plotHandle + theme(plot.title = titleFont$setFont())
  }
  if (!is.null(subtitleFont)) {
    plotHandle <- plotHandle + theme(plot.subtitle = subtitleFont$setFont())
  }
  if (!is.null(xAxisFont)) {
    plotHandle <- plotHandle + theme(axis.title.x = xAxisFont$setFont(), axis.text.x = xAxisFont$setFont())
  }
  if (!is.null(yAxisFont)) {
    plotHandle <- plotHandle + theme(axis.title.y = yAxisFont$setFont(), axis.text.y = yAxisFont$setFont())
  }
  if (!is.null(legendFont)) {
    plotHandle <- plotHandle + theme(legend.text = legendFont$setFont())
  }

  return(plotHandle)
}
