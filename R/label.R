#' @title Label
#' @description  R6 class defining `text` and `font` of labels
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    #' @description Create a new `Label` object.
    #' @param text character text of the label
    #' @param font `Font` object defining the font of the label
    #' @param size numeric defining the size of the label
    #' @param color character defining the color of the label
    #' @param fontFamily character defining the font family of the label
    #' @param fontFace character defining the font face of the label as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of the label.
    #' @param align character defining the alignment of the label as defined in helper enum `Alignments`.
    #' @return A new `Label` object
    initialize = function(text = "",
                          font = NULL,
                          color = NULL,
                          size = NULL,
                          fontFace = NULL,
                          fontFamily = NULL,
                          angle = NULL,
                          align = NULL) {
      validateIsNumeric(as.numeric(size, angle), nullAllowed = TRUE)
      validateIsString(c(color, fontFamily), nullAllowed = TRUE)
      validateIsOfType(font, "Font", nullAllowed = TRUE)
      validateIsIncluded(fontFace, FontFaces, nullAllowed = TRUE)
      validateIsIncluded(align, Alignments, nullAllowed = TRUE)

      self$text <- text
      self$font <- font %||% Font$new()
      # If font properties are explicitely written, they will overwrite the properties of input Font
      eval(.parseVariableToObject("self$font", c("size", "color", "fontFace", "fontFamily", "angle", "align"), keepIfNull = TRUE))
    },

    #' @description Create a `ggtext::element_textbox` directly convertible by `ggplot2::theme()`.
    #' @param size numeric defining the size of the label
    #' @param color character defining the color of the label
    #' @param fontFamily character defining the font family of the label
    #' @param fontFace character defining the font face of the label as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of the label.
    #' @param align character defining the alignment of the label as defined in helper enum `Alignments`.
    #' @return An `element_text` or `element_blank`object.
    createPlotTextBoxFont = function(color = NULL,
                                     size = NULL,
                                     fontFace = NULL,
                                     fontFamily = NULL,
                                     angle = NULL,
                                     align = NULL) {
      if (isEmpty(self$text)) {
        return(ggplot2::element_blank())
      }

      return(self$font$createPlotTextBoxFont(
        color = color,
        size = size,
        fontFace = fontFace,
        fontFamily = fontFamily,
        angle = angle,
        align = align
      ))
    },

    #' @description Create a `ggplot2::element_text()` directly convertible by `ggplot2::theme()`.
    #' @param size numeric defining the size of the label
    #' @param color character defining the color of the label
    #' @param fontFamily character defining the font family of the label
    #' @param fontFace character defining the font face of the label as defined in helper enum `FontFaces`.
    #' @param angle numeric defining the angle of the label.
    #' @param align character defining the alignment of the label as defined in helper enum `Alignments`.
    #' @return An `element_text` or `element_blank`object.
    createPlotTextFont = function(color = NULL,
                                  size = NULL,
                                  fontFace = NULL,
                                  fontFamily = NULL,
                                  angle = NULL,
                                  align = NULL) {
      if (isEmpty(self$text)) {
        return(ggplot2::element_blank())
      }
      return(self$font$createPlotTextFont(
        color = color,
        size = size,
        fontFace = fontFace,
        fontFamily = fontFamily,
        angle = angle,
        align = align
      ))
    }
  ),
  active = list(
    #' @field text character text of the label
    text = function(value) {
      if (missing(value)) {
        return(private$.text)
      }
      validateIsString(value, nullAllowed = TRUE)
      private$.text <- value
      return(invisible())
    },
    #' @field font `Font` object
    font = function(value) {
      if (missing(value)) {
        return(private$.font)
      }
      validateIsOfType(value, "Font", nullAllowed = TRUE)
      private$.font <- value %||% Font$new()
      # Ensures that size is numeric
      private$.font$size <- as.numeric(private$.font$size)
      return(invisible())
    }
  ),
  private = list(
    .text = NULL,
    .font = NULL
  )
)
