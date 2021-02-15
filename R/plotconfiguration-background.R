#' @title BackgroundConfiguration
#' @description R6 class defining the configuration of background
#' @export
BackgroundConfiguration <- R6::R6Class(
  "BackgroundConfiguration",
  public = list(
    #' @description Create a new \code{BackgroundConfiguration} object
    #' @param watermark \code{Label} object defining properties of watermark
    #' @param plot \code{BackgroundElement} object defining oustide plot background properties
    #' @param panel \code{BackgroundElement} object defining panel (inside of plot) background properties
    #' @param xAxis \code{LineElement} object defining properties of x-axis
    #' @param yAxis \code{LineElement} object defining properties of y-axis
    #' @param xGrid \code{LineElement} object defining properties of x-grid
    #' @param yGrid \code{LineElement} object defining properties of y-grid
    #' @return A new \code{BackgroundConfiguration} object
    initialize = function(watermark = NULL,
                              plot = NULL,
                              panel = NULL,
                              xAxis = NULL,
                              yAxis = NULL,
                              xGrid = NULL,
                              yGrid = NULL) {
      validateIsOfType(watermark, c("character", "Label"), nullAllowed = TRUE)
      watermark <- watermark %||% as.character(tlfEnv$currentTheme$background$watermark)
      # Enforce watermark as Label with value
      if (isOfType(watermark, "character")) {
        watermark <- asLabel(text = watermark, font = tlfEnv$currentTheme$fonts$watermark)
      }
      private$.watermark <- watermark

      areaFieldNames <- c("plot", "panel")
      lineFieldNames <- c("xAxis", "yAxis", "xGrid", "yGrid")

      validateAreaExpression <- parse(text = paste0("validateIsOfType(", areaFieldNames, ", 'BackgroundElement', nullAllowed = TRUE)"))
      validateLineExpression <- parse(text = paste0("validateIsOfType(", lineFieldNames, ", 'LineElement', nullAllowed = TRUE)"))
      eval(validateAreaExpression)
      eval(validateLineExpression)

      setAreaExpression <- parse(text = paste0("private$.", areaFieldNames, " <- ", areaFieldNames, " %||% tlfEnv$currentTheme$background$", areaFieldNames))
      setLineExpression <- parse(text = paste0("private$.", lineFieldNames, " <- ", lineFieldNames, " %||% tlfEnv$currentTheme$background$", lineFieldNames))
      eval(setAreaExpression)
      eval(setLineExpression)
    },

    #' @description Update background a \code{ggplot} object from `BackgroundConfiguration` properties
    #' @param plotObject a \code{ggplot} object
    #' @return A \code{ggplot} object
    updatePlot = function(plotObject) {
      plotObject <- plotObject + ggplot2::theme(
        plot.background = private$.plot$createPlotElement(),
        panel.background = private$.panel$createPlotElement(),
        axis.line.x = private$.xAxis$createPlotElement(),
        axis.line.y = private$.yAxis$createPlotElement(),
        panel.grid.major.x = private$.xGrid$createPlotElement(),
        panel.grid.major.y = private$.yGrid$createPlotElement()
      )
      return(plotObject)
    }
  ),
  active = list(
    #' @field watermark `Label` object
    watermark = function(value) {
      if (missing(value)) {
        return(private$.watermark)
      }
      validateIsOfType(value, c("character", "Label"), nullAllowed = TRUE)
      value <- value %||% private$.watermark$text
      # Enforce watermark as Label with value
      if (isOfType(value, "character")) {
        value <- asLabel(text = value, font = private$.watermark$font)
      }
      private$.watermark <- value
      return(invisible())
    },
    #' @field plot `BackgroundElement` object
    plot = function(value) {
      requestOnElement(private$.plot, value)
    },
    #' @field panel `BackgroundElement` object
    panel = function(value) {
      requestOnElement(private$.panel, value)
    },
    #' @field xAxis `LineElement` object
    xAxis = function(value) {
      requestOnElement(private$.xAxis, value)
    },
    #' @field yAxis `LineElement` object
    yAxis = function(value) {
      requestOnElement(private$.yAxis, value)
    },
    #' @field xGrid `LineElement` object
    xGrid = function(value) {
      requestOnElement(private$.xGrid, value)
    },
    #' @field yGrid `LineElement` object
    yGrid = function(value) {
      requestOnElement(private$.yGrid, value)
    }
  ),
  private = list(
    .watermark = NULL,
    .plot = NULL,
    .panel = NULL,
    .xAxis = NULL,
    .yAxis = NULL,
    .xGrid = NULL,
    .yGrid = NULL
  ),
)

#' @title BackgroundElement
#' @description  R6 class defining the properties of background elements
#' @field fill character defining the color filling of the background element
#' @field color character defining the color of the background element frame/line
#' @field size numeric defining the size of the background element frame/line
#' @field linetype character defining the size of the background element frame/line
#' @export
BackgroundElement <- R6::R6Class(
  "BackgroundElement",
  public = list(
    fill = NULL,
    color = NULL,
    size = NULL,
    linetype = NULL,

    #' @description Create a new \code{BackgroundElement} object
    #' @param fill character color filling of the background element
    #' @param color character color of the frame of the background element
    #' @param size character size of the frame of the background element
    #' @param linetype character linetype of the frame of the background element
    #' @return A new \code{BackgroundElement} object
    initialize = function(fill = NULL,
                              color = NULL,
                              size = NULL,
                              linetype = NULL) {
      validateIsString(c(fill, color, linetype), nullAllowed = TRUE)
      validateIsNumeric(size, nullAllowed = TRUE)

      fieldNames <- c("fill", "color", "size", "linetype")
      setPropertiesExpression <- parse(text = paste0("self$", fieldNames, " <- ", fieldNames))
      eval(setPropertiesExpression)
    },

    #' @description Create a `ggplot2::element_rect`  directly usable by `ggplot2::theme`.
    #' @return An `element_rect` object.
    createPlotElement = function() {
      ggplot2::element_rect(
        fill = self$fill,
        colour = self$color,
        size = as.numeric(self$size),
        linetype = self$linetype
      )
    }
  )
)

#' @title LineElement
#' @description  R6 class defining the properties of background line elements
#' @export
LineElement <- R6::R6Class(
  "LineElement",
  inherit = BackgroundElement,
  public = list(
    #' @description Create a `ggplot2::element_line` directly usable by `ggplot2::theme`.
    #' @return An `element_line` object.
    createPlotElement = function() {
      ggplot2::element_line(
        colour = self$color,
        size = as.numeric(self$size),
        linetype = self$linetype
      )
    }
  )
)

requestOnElement <- function(field, value) {
  if (missing(value)) {
    return(field)
  }
  # Update the element partially in case of names list
  if (isOfType(value, "list")) {
    for (fieldName in c("color", "size", "linetype")) {
      field[[fieldName]] <- value[[fieldName]] %||% field[[fieldName]]
    }
    if (isOfType(field, "BackgroundElement")) {
      field[["fill"]] <- value[["fill"]] %||% field[["fill"]]
    }
  }
  # Or update the whole element R6 object is used
  if (isOfType(value, "BackgroundElement")) {
    field <- value
  }
}
