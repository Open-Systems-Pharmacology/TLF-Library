#' @title Font
#' @docType class
#' @description Font properties for ggplot labels
#' @field size numeric
#' @field color character
#' @field fontFamily character
#' @field fontFace character
#' @section Methods:
#' \describe{
#' \item{new(...)}{Initialize Font}
#' \item{print(...)}{Print Font properties}
#' \item{setFont()}{Create element_text for ggplot with Font properties}
#' }
#' @include utils.R
#' @export
Font <- R6::R6Class(
  "Font",
  public = list(
    # Defining default font properties here fixes
    # default input that can be overwritten by NULL,
    # which leads to empty font property field
    size = 12,
    color = "black",
    fontFamily = "",
    fontFace = "plain",

    initialize = function(size = NULL,
                              color = NULL,
                              fontFamily = NULL,
                              fontFace = NULL) {
      self$size <- size %||% self$size
      self$color <- color %||% self$color
      self$fontFamily <- fontFamily %||% self$fontFamily
      self$fontFace <- fontFace %||% self$fontFace
    },

    print = function() {
      cat("size:", self$size, "\n", sep = " ")
      cat("color:", self$color, "\n", sep = " ")
      cat("fontFamily:", self$fontFamily, "\n", sep = " ")
      cat("fontFace:", self$fontFace, "\n", sep = " ")
      invisible(self)
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
