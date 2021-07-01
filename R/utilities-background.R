#' @title setGrid
#' @description Set x and y grid properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param color character color of the grid
#' @param linetype character linetype of the grid. Use "blank" to remove grid.
#' @param size numeric size of the grid lines
#' @return ggplot object
#' @export
setGrid <- function(plotObject,
                    color = NULL,
                    linetype = NULL,
                    size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$xGrid", c("color", "linetype", "size"), keepIfNull = TRUE))
  eval(parseVariableToObject("background$yGrid", c("color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setXGrid
#' @description Set x-grid properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param color character color of the grid
#' @param linetype character linetype of the grid. Use "blank" to remove grid.
#' @param size numeric size of the grid lines
#' @return ggplot object
#' @export
setXGrid <- function(plotObject,
                     color = NULL,
                     linetype = NULL,
                     size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$xGrid", c("color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setYGrid
#' @description Set x and y grid properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param color character color of the grid
#' @param linetype character linetype of the grid. Use "blank" to remove grid.
#' @param size numeric size of the grid lines
#' @return ggplot object
#' @export
setYGrid <- function(plotObject,
                     color = NULL,
                     linetype = NULL,
                     size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$yGrid", c("color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setBackground
#' @description Set background properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param fill character color fill of the background
#' @param color character color of the background frame
#' @param linetype character linetype of the background frame
#' @param size numeric size of the background frame
#' @return ggplot object
#' @export
setBackground <- function(plotObject,
                          fill = NULL,
                          color = NULL,
                          linetype = NULL,
                          size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(fill, nullAllowed = TRUE)
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$plot", c("fill", "color", "linetype", "size"), keepIfNull = TRUE))
  eval(parseVariableToObject("background$panel", c("fill", "color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setBackgroundPanelArea
#' @description Set background panel area properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param fill character color fill of the background
#' @param color character color of the background frame
#' @param linetype character linetype of the background frame
#' @param size numeric size of the background frame
#' @return ggplot object
#' @export
setBackgroundPanelArea <- function(plotObject,
                                   fill = NULL,
                                   color = NULL,
                                   linetype = NULL,
                                   size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(fill, nullAllowed = TRUE)
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$panel", c("fill", "color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title setBackgroundPlotArea
#' @description Set background plot area properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param fill character color fill of the background
#' @param color character color of the background frame
#' @param linetype character linetype of the background frame
#' @param size numeric size of the background frame
#' @return ggplot object
#' @export
setBackgroundPlotArea <- function(plotObject,
                                  fill = NULL,
                                  color = NULL,
                                  linetype = NULL,
                                  size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(fill, nullAllowed = TRUE)
  validateIsString(color, nullAllowed = TRUE)
  validateIsIncluded(linetype, Linetypes, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  background <- newPlotObject$plotConfiguration$background
  eval(parseVariableToObject("background$plot", c("fill", "color", "linetype", "size"), keepIfNull = TRUE))
  newPlotObject <- background$updatePlot(newPlotObject)
  return(newPlotObject)
}

#' @title addWatermark
#' @param plotObject ggplot object to which the watermark is added
#' @param watermark Character or `Label` object corresponding to the watermark text
#' (and its font properties if `Label`)
#' @param color Color of the watermark label.
#' @param size Size of the watermark label.
#' @param angle Angle of the watermark label (in degree).
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' @return \code{plotObject} ggplot object to which the watermark is added.
#' @description
#' addWatermark creates a ggplot grob based on the label text and its font properties.
#' Then,  adds the grob to the ggplot object input \code{plotObject} as a new layer using \code{ggplot2::annotation_custom}.
#' @import  ggplot2
#' @export
#' @examples
#' p <- ggplot2::ggplot()
#' addWatermark(p, "watermark")
#'
#' # Watermark with font properties
#' watermarkLabel <- Label$new(text = "watermark", color = "blue")
#' addWatermark(p, watermarkLabel)
#'
#' # Horizontal watermark
#' addWatermark(p, watermarkLabel, angle = 0)
#'
#' # Watermark totally opaque
#' addWatermark(p, watermarkLabel, alpha = 1)
#'
#' # Create a sun as background
#' for (angle in seq(0, 340, 20)) {
#'   p <- addWatermark(p,
#'     watermark = "            >",
#'     color = "yellow", angle = angle, alpha = 1
#'   )
#' }
addWatermark <- function(plotObject,
                         watermark,
                         color = NULL,
                         size = NULL,
                         angle = NULL,
                         alpha = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(watermark, c("character", "Label"))
  validateIsString(color, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)
  validateIsNumeric(alpha, nullAllowed = TRUE)
  validateIsNumeric(angle, nullAllowed = TRUE)
  # Transparency from theme aesthetic map if left undefined
  alpha <- alpha %||% getAestheticValues(
    n = 1,
    selectionKey = AestheticSelectionKeys$first,
    aesthetic = "alpha"
  )
  # Ensure watermark is a Label class
  if (isOfType(watermark, "character")) {
    watermark <- asLabel(watermark, font = tlfEnv$currentTheme$fonts$watermark)
  }
  eval(parseVariableToObject("watermark$font", c("color", "size", "angle"), keepIfNull = TRUE))

  watermark <- createWatermarkGrob(label = watermark, alpha = alpha)

  plotObject <- plotObject + ggplot2::annotation_custom(grob = watermark, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf)

  return(plotObject)
}

#' @title setWatermark
#' @param plotObject ggplot object to which the watermark is set
#' @param watermark Character or `Label` object corresponding to the watermark text
#' (and its font properties if `Label`)
#' @param color Color of the watermark label.
#' @param size Size of the watermark label.
#' @param angle Angle of the watermark label (in degree).
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' @return ggplot object to which the watermark is added.
#' @import ggplot2
#' @export
setWatermark <- function(plotObject,
                         watermark = NULL,
                         color = NULL,
                         size = NULL,
                         angle = NULL,
                         alpha = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(watermark, c("character", "Label"), nullAllowed = TRUE)
  validateIsString(color, nullAllowed = TRUE)
  validateIsNumeric(size, nullAllowed = TRUE)
  validateIsNumeric(alpha, nullAllowed = TRUE)
  validateIsNumeric(angle, nullAllowed = TRUE)

  alpha <- alpha %||% getAestheticValues(
    n = 1,
    selectionKey = AestheticSelectionKeys$first,
    aesthetic = "alpha"
  )

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$background$watermark
  watermarkConfiguration <- newPlotObject$plotConfiguration$background$watermark

  if (isOfType(watermark, "character")) {
    watermark <- asLabel(text = watermark, font = watermarkConfiguration$font)
  }
  watermarkConfiguration <- watermark %||% watermarkConfiguration
  eval(parseVariableToObject("watermarkConfiguration$font", c("color", "size", "angle"), keepIfNull = TRUE))

  # If plot is initialized, addWatermark otherwise update watermark
  if (isOfLength(plotObject$layers, 0)) {
    newPlotObject <- addWatermark(newPlotObject,
      watermark = watermarkConfiguration,
      alpha = alpha
    )
    return(newPlotObject)
  }

  # Using initializePlot, watermark will always be as first layer
  dummyPlot <- addWatermark(ggplot2::ggplot(),
    watermark = watermarkConfiguration,
    alpha = alpha
  )
  newPlotObject$layers[[1]] <- dummyPlot$layer[[1]]
  return(newPlotObject)
}

#' @title createWatermarkGrob
#' @param label Character or Label object corresponding to the watermark text
#' (and its font properties if Label)
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' Default alpha is defined from theme aesthetic alpha map
#' @return Watermark background as a ggplot grob object
#' @description
#' createWatermarkGrob creates a ggplot grob based on the label text and its font properties.
#' @export
createWatermarkGrob <- function(label, alpha = NULL) {
  validateIsNumeric(alpha, nullAllowed = TRUE)
  # Ensure label is a Label class
  label <- asLabel(label)
  alpha <- alpha %||% getAestheticValues(
    n = 1,
    selectionKey = AestheticSelectionKeys$first,
    aesthetic = "alpha"
  )

  watermark <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = label$text %||% "",
      color = label$font$color, fontface = label$font$fontFace, size = label$font$size,
      angle = label$font$angle, alpha = alpha
    )
  watermark <- ggplot2::ggplotGrob(watermark)

  return(watermark)
}
