#' @title Font
#' @description  R6 class defining font properties
#' @field size numeric defining the size of font
#' @field color character defining the color of font
#' @field fontFamily character defining the family of font
#' @field fontFace character defining the font face as defined in helper enum `FontFaces`.
#' @field angle numeric defining the angle of font
#' @field maxwidth `unit()` object defining the maximum width of text.
#' @field margin `ggplot2::margin()` object defining the space around text.
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
    maxwidth = NULL,
    margin = ggplot2::margin(6,4,6,4, "pt"),
    #' @description Create a new `Font` object.
    #' Default font properties are defined directly in the object field,
    #' so `NULL` input is allowed will lead to default properties.
    #' @param color character defining the color of font.
    #' @param size numeric defining the size of font.
    #' @param fontFamily character defining the family of font.
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font.
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @param maxwidth `unit()` object defining the maximum width of text.
    #' @param margin `ggplot2::margin()` object defining the space around text.
    #' @return A new `Font` object
    initialize = function(color = NULL,
                          size = NULL,
                          fontFamily = NULL,
                          fontFace = NULL,
                          angle = NULL,
                          align = NULL,
                          maxwidth = NULL,
                          margin = NULL) {
      validateIsString(c(color, fontFamily), nullAllowed = TRUE)
      validateIsNumeric(c(size, angle), nullAllowed = TRUE)
      validateIsIncluded(fontFace, FontFaces, nullAllowed = TRUE)
      validateIsIncluded(align, Alignments, nullAllowed = TRUE)
      eval(.parseVariableToObject("self", c("size", "color", "fontFace", "fontFamily", "angle", "align"), keepIfNull = TRUE))
    },

    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font.
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @param margin `ggplot2::margin()` object defining the space around text.
    #' @return An `element_text` object.
    createPlotTextFont = function(size = NULL,
                                  color = NULL,
                                  fontFamily = NULL,
                                  fontFace = NULL,
                                  angle = NULL,
                                  align = NULL,
                                  margin = NULL) {
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
                       "right" = 1),
        margin = margin %||% self$margin
      )
    },
    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @param size numeric defining the size of font
    #' @param color character defining the color of font
    #' @param fontFamily character defining the family of font
    #' @param fontFace character defining the font face as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of font.
    #' @param align character defining the alignment of font as defined in helper enum `Alignments`.
    #' @param maxwidth `unit()` object defining the maximum width of text.
    #' @param margin `ggplot2::margin()` object defining the space around text.
    #' @return An `ggtext::element_textbox` object.
    createPlotTextBoxFont = function(size = NULL,
                                     color = NULL,
                                     fontFamily = NULL,
                                     fontFace = NULL,
                                     angle = NULL,
                                     align = NULL,
                                     maxwidth = NULL,
                                     margin = NULL) {

      ggtext::element_textbox_simple(
        colour = color %||% self$color,
        size = size %||% self$size,
        face = fontFace %||% self$fontFace,
        # Use font family only if available in Windows font database database
        family = .checkPlotFontFamily(fontFamily %||% self$fontFamily),
        orientation = .convertAngleToOrientation(angle %||% self$angle),
        valign = 0.5,
        halign = switch(align %||% self$align,
                        "left" = 0,
                        "center" = 0.5,
                        "right" = 1),
        maxwidth = maxwidth %||% self$maxwidth,
        margin = margin %||% self$margin
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

.convertAngleToOrientation <- function(angle){
  return(
    switch(as.character(angle),
           "0" = "upright",
           "90" = "left-rotated",
           "180" = "right-rotated",
           "270" = "inverted")
  )
}
