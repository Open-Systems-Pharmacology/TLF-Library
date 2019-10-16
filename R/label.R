#' @title Label
#' @docType class
#' @description
#' Label class associate font to a text
#' @include utils.R
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    text = NULL,
    font = NULL,

    initialize = function(text = "", font = NULL) {
      self$text <- text
      validateIsOfType(font, Font, nullAllowed = TRUE)
      self$font <- font %||% Font$new()
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
