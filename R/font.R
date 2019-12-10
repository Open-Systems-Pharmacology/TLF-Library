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
