#' @title Font
#' @docType class
#' @description
#' Font class defining properties of a text (size, color, font)
#' @include utils.R
#' @export
Font <- R6::R6Class(
  "Font",
  public = list(
    size = NULL,
    color = NULL,
    fontFamily = NULL,
    fontFace = NULL,

    initialize = function(size = 12,
                          color = "black",
                          fontFamily = "",
                          fontFace = "plain") {
      self$size <- size
      self$color <- color
      self$fontFamily <- fontFamily
      self$fontFace <- fontFace
    },

    setFont = function() {
      element_text(
        colour = self$color,
        size = self$size,
        face = self$fontFace,
        family = self$fontFamily
      )
    }
  )
)

#' @title Label
#' @docType class
#' @description
#' Label class associate font to a text
#' @include utils.R
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    "text" = "",
    "font" = NULL,

    initialize = function(text = "",
                          font = NULL) {
      self$text <- text
      font <- font %||% Font$new()

      stopifnot("Font" %in% class(font))
      self$font <- font
    },

    setFontProperties = function(color = self$font$color,
                                 size = self$font$size,
                                 fontFamily = self$font$fontFamily,
                                 fontFace = self$font$fontFace) {
      self$font$color <- color
      self$font$size <- size
      self$font$fontFamily <- fontFamily
      self$font$fontFace <- fontFace
    },

    print = function() {
      print(self$text)
      invisible(self)
    }
  )
)