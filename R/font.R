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
