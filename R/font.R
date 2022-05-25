#' @title Font
#' @description  R6 class defining font properties
#' @field size numeric defining the size of font
#' @field color character defining the color of font
#' @field fontFamily character defining the family of font
#' @field fontFace character defining the font face as defined in helper enum `FontFaces`.
#' @field angle numeric defining the angle of font
#' @field align character defining the alignment of font as defined in helper enum `Alignments`.
#' @export
Font <- R6::R6Class(
  "Font",
  public = list(
    size = 12,
    color = "black",
    fontFamily = "",
    fontFace = "plain",
    angle = 0,
    align = "center",

    #' @description Create a new `Font` object.
    #' Default font properties are defined directly in the object field,
    #' so `NULL` input is allowed will lead to default properties.
    #' @param color character defining the color of font
    #' @param size numeric defining the size of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @return A new `Font` object
    initialize = function(color = NULL,
                          size = NULL,
                          fontFamily = NULL,
                          fontFace = NULL,
                          angle = NULL,
                          align = NULL) {
      validateIsString(c(color, fontFamily), nullAllowed = TRUE)
      validateIsNumeric(c(size, angle), nullAllowed = TRUE)
      validateIsIncluded(fontFace, FontFaces, nullAllowed = TRUE)
      validateIsIncluded(align, Alignments, nullAllowed = TRUE)
      eval(parseVariableToObject("self", c("size", "color", "fontFace", "fontFamily", "angle", "align"), keepIfNull = TRUE))
    },

    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @return An `element_text` object.
    createPlotFont = function(size = NULL,
                              color = NULL,
                              fontFamily = NULL,
                              fontFace = NULL,
                              angle = NULL,
                              align = NULL) {
      ggplot2::element_text(
        colour = color %||% self$color,
        size = size %||% self$size,
        face = fontFace %||% self$fontFace,
        # Use font family only if available in Windows font database database
        family = checkPlotFontFamily(fontFamily %||% self$fontFamily),
        angle = angle %||% self$angle,
        vjust = 0.5,
        hjust = switch(align %||% self$align,
          "left" = 0,
          "center" = 0.5,
          "right" = 1
        )
      )
    }
  )
)

#' @title Alignments
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available alignments/justifications for fonts
#' @family enum helpers
Alignments <- enum(c("left", "center", "right"))

#' @title FontFaces
#' @import ospsuite.utils
#' @export
#' @description
#' List of all available font faces
#' @family enum helpers
FontFaces <- enum(c("plain", "bold", "italic", "bold.italic"))


#' @title checkPlotFontFamily
#' @description Check if font family is available in Windows font database.
#' Use function `grDevices::windowsFonts()` to get the list of font families available.
#' @param fontFamily character defining the family of font
#' @return Name of font family if available in Windows font database
#' `NULL` otherwise
#' @keywords internal
checkPlotFontFamily <- function(fontFamily) {
  if (isEmpty(.Platform$OS.type)) {
    return(NULL)
  }
  if (.Platform$OS.type != "windows") {
    return(NULL)
  }
  if (isIncluded(fontFamily, names(grDevices::windowsFonts()))) {
    return(fontFamily)
  }
  return(NULL)
}
