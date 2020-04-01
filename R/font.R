#' @title Font
#' @description  R6 class defining \code{size}, \code{color}, \code{fontFamily}, \code{fontFace} of font
#' @include utils.R
#' @export
Font <- R6::R6Class(
  "Font",
  public = list(
    #' @field size numeric size of font
    size = 12,
    #' @field color character color of font
    color = "black",
    #' @field fontFamily character family of font
    fontFamily = "",
    #' @field fontFace character face of font
    fontFace = "plain",
    
    #' @description Create a new \code{Font} object.
    #' Default font properties are defined directly in the object field,
    #' so `NULL` input is allowed will lead to default properties.
    #' @param size numeric size of font
    #' @param color character color of font
    #' @param fontFamily character family of font
    #' @param fontFace character face of font
    #' @return A new \code{Font} object
    initialize = function(size = NULL,
                              color = NULL,
                              fontFamily = NULL,
                              fontFace = NULL) {
      self$size <- size %||% self$size
      self$color <- color %||% self$color
      self$fontFamily <- fontFamily %||% self$fontFamily
      self$fontFace <- fontFace %||% self$fontFace
    },

    #' @description Print \code{Font} properties.
    print = function() {
      cat("size:", self$size, "\n", sep = " ")
      cat("color:", self$color, "\n", sep = " ")
      cat("fontFamily:", self$fontFamily, "\n", sep = " ")
      cat("fontFace:", self$fontFace, "\n", sep = " ")
      invisible(self)
    },

    #' @description Create an `element_text` for ggplot with \code{Font} properties.
    #' @return An `element_text` for ggplot with \code{Font} properties.
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
