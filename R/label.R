#' @title Label
#' @docType class
#' @description Properties for ggplot labels
#' @field text character
#' @field font Font object
#' @section Methods:
#' \describe{
#' \item{new(...)}{Initialize Label}
#' \item{print(...)}{Print Label properties}
#' \item{setFontProperties(color, size, fontFamily, fontFace)}{Set fields of font object}
#' }
#' @include utils.R
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    text = NULL,
    font = NULL,

    initialize = function(text = "",
                              font = NULL,
                              color = NULL,
                              size = NULL,
                              fontFace = NULL,
                              fontFamily = NULL) {
      self$text <- text
      validateEitherOrNullInput(
        list("font" = font),
        list(
          "color" = color,
          "size" = size,
          "fontFace" = fontFace,
          "fontFamily" = fontFamily
        )
      )

      self$font <- font %||% Font$new(
        color = color,
        size = size,
        fontFace = fontFace,
        fontFamily = fontFamily
      )
      validateIsOfType(self$font, Font)
    },

    print = function() {
      cat("text:", self$text, "\n", sep = " ")
      cat("font:", self$font, "\n", sep = " ")
      invisible(self)
    },

    setFontProperties = function(color = self$font$color,
                                     size = self$font$size,
                                     fontFamily = self$font$fontFamily,
                                     fontFace = self$font$fontFace) {
      self$font$color <- color
      self$font$size <- size
      self$font$fontFamily <- fontFamily
      self$font$fontFace <- fontFace
    }
  )
)
