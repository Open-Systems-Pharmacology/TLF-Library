#' @title Font
#' @description  R6 class defining font properties
#' @field size numeric defining the size of font
#' @field color character defining the color of font
#' @field fontFamily character defining the family of font
#' @field fontFace character defining the font face as defined in helper enum `FontFaces`.
#' @field angle numeric defining the angle of font
#' @field orientation character defining the orientation of the text as defined in the helper enum `LabelOrientations`.
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
    orientation = NULL,
    align = "center",
    #' @description Create a new `Font` object.
    #' Default font properties are defined directly in the object field,
    #' so `NULL` input is allowed will lead to default properties.
    #' @param color character defining the color of font.
    #' @param size numeric defining the size of font.
    #' @param fontFamily character defining the family of font.
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font.
    #' @param orientation character defining the orientation of the text as defined in the helper enum `LabelOrientations`.
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @return A new `Font` object
    initialize = function(color = NULL,
                          size = NULL,
                          fontFamily = NULL,
                          fontFace = NULL,
                          angle = NULL,
                          orientation = NULL,
                          align = NULL) {
      validateIsString(c(color, fontFamily), nullAllowed = TRUE)
      validateIsNumeric(c(size, angle), nullAllowed = TRUE)
      validateIsIncluded(fontFace, FontFaces, nullAllowed = TRUE)
      validateIsIncluded(align, Alignments, nullAllowed = TRUE)
      validateIsIncluded(orientation, LabelOrientations, nullAllowed = TRUE)
      eval(.parseVariableToObject("self", c("size", "color", "fontFace", "fontFamily", "angle", "orientation", "align"), keepIfNull = TRUE))
    },

    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @return An `element_text` object.
    createPlotTextFont = function(size = NULL,
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
        family = .checkPlotFontFamily(fontFamily %||% self$fontFamily),
        angle = angle %||% self$angle,
        vjust = 0.5,
        hjust = switch(align %||% self$align,
          "left" = 0,
          "center" = 0.5,
          "right" = 1
        ),
        margin = unit(unit(c(2,2,2,2), "pt"))
      )
    },
    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param orientation character defining the orientation of the text as defined in the helper enum `LabelOrientations`.
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @return An `element_text` object.
    createPlotTextBoxFont = function(size = NULL,
                                  color = NULL,
                                  fontFamily = NULL,
                                  fontFace = NULL,
                                  orientation = NULL,
                                  align = NULL) {
      ggtext::element_textbox_simple(
        colour = color %||% self$color,
        size = size %||% self$size,
        face = fontFace %||% self$fontFace,
        # Use font family only if available in Windows font database database
        family = .checkPlotFontFamily(fontFamily %||% self$fontFamily),
        orientation = size %||% self$orientation,
        valign = 0.5,
        halign = switch(align %||% self$align,
                       "left" = 0,
                       "center" = 0.5,
                       "right" = 1),
        margin = unit(c(10,5,10,5), "pt")
      )
    }
  )
)

#' @title .checkPlotFontFamily
#' @description Check if font family is available in Windows font database.
#' Use function `grDevices::windowsFonts()` to get the list of font families available.
#' @param fontFamily character defining the family of font
#' @return Name of font family if available in Windows font database
#' `NULL` otherwise
#' @keywords internal
.checkPlotFontFamily <- function(fontFamily) {
  # If showtext is installed, use its default font or user-defined font
  if (requireNamespace("showtext", quietly = TRUE)) {
    if (isIncluded(fontFamily, sysfonts::font_families())) {
      return(fontFamily)
    }
    return("NotoSans")
  }
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
