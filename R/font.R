#' @title Font
#' @description  R6 class defining font properties
#' @field size numeric defining the size of font
#' @field color character defining the color of font
#' @field fontFamily character defining the family of font
#' @field fontFace character defining the face of font
#' @field angle numeric defining the angle of font
#' @export
Font <- R6::R6Class(
  "Font",
  public = list(
    size = 12,
    color = "black",
    fontFamily = "",
    fontFace = "plain",
    angle = 0,

    #' @description Create a new \code{Font} object.
    #' Default font properties are defined directly in the object field,
    #' so `NULL` input is allowed will lead to default properties.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the face of font
    #' @param angle numeric defining the angle of font
    #' @return A new \code{Font} object
    initialize = function(size = NULL,
                              color = NULL,
                              fontFamily = NULL,
                              fontFace = NULL,
                              angle = NULL) {
      validateIsString(c(color, fontFamily, fontFace), nullAllowed = TRUE)
      validateIsNumeric(c(size, angle), nullAllowed = TRUE)

      fieldNames <- c("size", "color", "fontFace", "fontFamily", "angle")
      setFontExpression <- parse(text = paste0("self$", fieldNames, " <- ", fieldNames, " %||% self$", fieldNames))
      eval(setFontExpression)
    },

    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @return An `element_text` object.
    createPlotFont = function() {
      ggplot2::element_text(
        colour = self$color,
        size = self$size,
        face = self$fontFace,
        # TO DO: check why I get the following error messages
        # "font family not found in Windows font database"
        #family = self$fontFamily,
        angle = self$angle
      )
    }
  )
)
