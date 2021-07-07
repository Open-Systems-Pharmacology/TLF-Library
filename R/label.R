#' @title Label
#' @description  R6 class defining \code{text} and \code{font} of label
#' @export
Label <- R6::R6Class(
  "Label",
  public = list(
    #' @description Create a new \code{Label} object.
    #' @param text character text of the \code{Label} object
    #' @param font \code{Font} object defining the font of the `Label` object
    #' @param size numeric defining the size of the `Label` object
    #' @param color character defining the color of the `Label` object
    #' @param fontFamily character defining the font family of the `Label` object
    #' @param fontFace character defining the font face of the `Label` object
    #' @param angle numeric defining the angle of the `Label` object
    #' @return A new \code{Label} object
    initialize = function(text = "",
                              font = NULL,
                              color = NULL,
                              size = NULL,
                              fontFace = NULL,
                              fontFamily = NULL,
                              angle = NULL) {
      validateIsNumeric(c(as.numeric(angle), as.numeric(size)), nullAllowed = TRUE)
      validateIsString(c(color, fontFace, fontFamily), nullAllowed = TRUE)
      validateIsOfType(font, "Font", nullAllowed = TRUE)

      self$text <- text
      self$font <- font %||% Font$new()
      # If font properties are explicitely written, they will overwrite the properties of input Font
      eval(parseVariableToObject("self$font", c("size", "color", "fontFace", "fontFamily", "angle"), keepIfNull = TRUE))
    },

    #' @description Create a `ggplot2::element_text` directly convertible by `ggplot2::theme`.
    #' @return An `element_text` or `element_blank`object.
    createPlotFont = function() {
      if (isOfLength(self$text, 0)) {
        return(ggplot2::element_blank())
      }
      return(self$font$createPlotFont())
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
    #' @field font \code{Font} object
    font = function(value) {
      if (missing(value)) {
        return(private$.font)
      }
      validateIsOfType(value, "Font", nullAllowed = TRUE)
      private$.font <- value %||% Font$new()
      # Ensures that size and angle are numeric
      private$.font$size <- as.numeric(private$.font$size)
      private$.font$angle <- as.numeric(private$.font$angle)
      return(invisible())
    }
  ),
  private = list(
    .text = NULL,
    .font = NULL
  )
)
