# Utils fonctions to go along with fonts and labels
#' @title asLabel
#' @param text text to be set as label class
#' @param font font properties of Label
#' @return Label
#' @description
#' Set a character class into a Label class
#' Associate font if input
#' If text is already Label class return it and update font if input
#' @export
#' @examples
#' title <- "Title of Plot"
#' title <- asLabel(title)
asLabel <- function(text = "", font = NULL) {
  # Check input
  stopifnot("Label" %in% class(text) || "character" %in% class(text))

  # If text is already a Label class
  if (("character" %in% class(text))) {
    text <- Label$new(text)
  }

  # If font is not null, set font properties of Label
  if (!is.null(font)) {
    stopifnot("Font" %in% class(font))
    text$font <- font
  }

  return(text)
}

# Example of function to set a Title, TO BE DISCUSSED
# theme expected to be enum
asTitle <- function(text = "", theme = "default") {
  if (theme %in% "default") {
    font <- Font$new(size = 20, color = "blue", fontFace = "bold", fontFamily = "")
    title <- asLabel(text, font)
  }
  if (theme %in% "TLF") {
    font <- Font$new(size = 20, color = "firebrick4", fontFace = "bold", fontFamily = "")
    title <- asLabel(text, font)
  }
  return(title)
}