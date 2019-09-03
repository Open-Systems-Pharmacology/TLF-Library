#' @title Font Class
#' @docType class
#' @description
#' Font class defining properties of a text (size, color, font)
#' @export
FontClass <- R6::R6Class(
  "Font",
  public = list(
    "size" = 12,
    "color" = "black",
    "fontfamily" = "",
    "fontface" = "plain",

    initialize = function(size = 12,
                          color = "black",
                          fontfamily = "",
                          fontface = "plain") {
      self$size <- size
      self$color <- color
      self$fontfamily <- fontfamily
      self$fontface <- fontface
    },

    setFont = function() {
      element_text(
        colour = self$color,
        size = self$size,
        face = self$fontface,
        family = self$fontfamily
      )
    }
  )
)

LabelClass <- R6::R6Class(
  "Label",
  public = list(
    "text" = "",
    "font" = NULL,

    initialize = function(text = "",
                          font = NULL) {
      self$text <- text
      font <- font %||% FontClass$new()

      stopifnot("Font" %in% class(font))
      self$font <- font
    },

    setFontProperties = function(color=self$font$color, 
                                 size=self$font$size, 
                                 fontfamily=self$font$fontfamily, 
                                 fontface=self$font$fontface) {
      self$font$color <- color
      self$font$size <- size
      self$font$fontfamily <- fontfamily
      self$font$fontface <- fontface
    },

    print = function() {
      print(self$text)
      invisible(self)
    }
  )
)

#' @title asLabel
#' @param text text to set as label class
#' @return Label
#' @description
#' set a character type into a Label class
#' @export
#' @examples
#' title <- "Title of Plot"
#' title <- asLabel(title)
asLabel <- function(text) {
  Label <- LabelClass$new(text)
  return(Label)
}