#' @title Label
#' @description  R6 class defining \code{text} and \code{font} of label
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    #' @field text character text of label
    text = NULL,
    #' @field font R6 class \code{Font} object
    font = NULL,

    #' @description Create a new \code{Label} object.
    #' @param text character text of label
    #' @param font R6 class \code{Font} object
    #' @param size numeric size of font
    #' @param color character color of font
    #' @param fontFamily character family of font
    #' @param fontFace character face of font
    #' @return A new \code{Label} object
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

    #' @description Print \code{Label} properties.
    print = function() {
      cat("text:", self$text, "\n", sep = " ")
      cat("font color:", self$font$color, "\n", sep = " ")
      cat("font size:", self$font$size, "\n", sep = " ")
      cat("font family:", self$font$fontFamily, "\n", sep = " ")
      cat("font face:", self$font$fontFace, "\n", sep = " ")
      invisible(self)
    },

    #' @description Set font properties of Label
    #' @param size numeric size of font
    #' @param color character color of font
    #' @param fontFamily character family of font
    #' @param fontFace character face of font
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
