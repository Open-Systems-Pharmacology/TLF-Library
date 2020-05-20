#' @title setGrid
#' @description Set grid properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param color character color of the grid
#' @param linetype character linetype of the grid. Use "blank" to remove grid.
#' @param size numeric size of the grid lines
#' @return ggplot object with updated Y-axis
#' @export
setGrid <- function(plotObject,
                    color = NULL,
                    linetype = NULL,
                    size = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(color, "character", nullAllowed = TRUE)
  validateIsOfType(linetype, "character", nullAllowed = TRUE)
  validateIsOfType(size, "numeric", nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$grid
  grid <- newPlotObject$plotConfiguration$background$grid

  grid$color <- color %||% grid$color
  grid$linetype <- linetype %||% grid$linetype
  grid$size <- size %||% grid$size

  newPlotObject <- newPlotObject + theme(
    panel.grid = element_line(
      color = grid$color,
      size = grid$size,
      linetype = grid$linetype
    )
  )

  return(newPlotObject)
}

#' @title setBackground
#' @description Set background properties on a ggplot object
#' @param plotObject ggplot object to set
#' @param fill character color fill of the background
#' @param color character color of the background frame
#' @param linetype character linetype of the background frame
#' @param size numeric size of the background frame
#' @param outerBackgroundFill character color fill of the outerBackground
#' @return ggplot object with updated Y-axis
#' @export
setBackground <- function(plotObject,
                          fill = NULL,
                          color = NULL,
                          linetype = NULL,
                          size = NULL,
                          outerBackgroundFill = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(size, "numeric", nullAllowed = TRUE)
  inputs <- c("fill", "color", "linetype", "outerBackgroundFill")
  validateExpressions <- parse(text = paste0("validateIsOfType(", inputs, ', "character", nullAllowed =TRUE)'))
  eval(validateExpressions)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration
  innerBackground <- newPlotObject$plotConfiguration$background$innerBackground
  outerBackground <- newPlotObject$plotConfiguration$background$outerBackground

  innerBackground$fill <- fill %||% innerBackground$fill
  innerBackground$color <- color %||% innerBackground$color
  innerBackground$linetype <- linetype %||% innerBackground$linetype
  innerBackground$size <- size %||% innerBackground$size
  outerBackground$fill <- outerBackgroundFill %||% innerBackground$fill

  newPlotObject <- newPlotObject + theme(
    plot.background = element_rect(fill = outerBackground$fill),
    legend.background = element_rect(fill = outerBackground$fill),
    panel.background = element_rect(
      fill = innerBackground$fill,
      color = innerBackground$color,
      size = innerBackground$size,
      linetype = innerBackground$linetype,
    )
  )
  return(newPlotObject)
}


#' @title addWatermark
#' @param plotObject ggplot object to which the watermark is added
#' @param label Character or Label class object corresponding to the watermark text
#' (and its font properties if Label)
#' @param angle Angle in degree from horizontal of the watermark label. Default angle is 30 degrees.
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' Default alpha is 0.4.
#' @return \code{plotObject} ggplot object to which the watermark is added.
#' @description
#' addWatermark creates a ggplot grob based on the label text and its font properties.
#' Then,  adds the grob to the ggplot object input \code{plotObject} as a new layer using \code{ggplot2::annotation_custom}.
#' \code{angle} and \code{alpha} are optional input to customize the angle and transparency of the watermark text.
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
#'     label = Label$new(text = "            >", color = "yellow"),
#'     angle = angle, alpha = 1
#'   )
#' }
addWatermark <- function(plotObject,
                         label,
                         angle = 30,
                         alpha = 0.4) {
  validateIsOfType(plotObject, "ggplot")
  # Ensure label is a Label class
  label <- asLabel(label)

  watermark <- createWatermarkGrob(label = label, angle = angle, alpha = alpha)

  plotObject <- plotObject + ggplot2::annotation_custom(grob = watermark, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf)

  return(plotObject)
}

#' @title setWatermark
#' @param plotObject ggplot object to which the watermark is set
#' @param watermark character or Label class object
#' @param angle Angle in degree from horizontal of the watermark label. Default angle is 30 degrees.
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' Default alpha is 0.6.
#' @return ggplot object to which the watermark is added.
#' @import  ggplot2
#' @export
setWatermark <- function(plotObject,
                         watermark = NULL,
                         angle = 30,
                         alpha = 0.4) {
  validateIsOfType(plotObject, "ggplot")
  # Angle and alpha can be added as a part of plot configuration later on:
  # For this create Watermark R6 class inheriting from Label and which gets fields angle and alpha
  validateIsOfType(watermark, c("Label", "character"), nullAllowed = TRUE)

  # Clone plotConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$background$watermark
  watermarkConfiguration <- newPlotObject$plotConfiguration$background$watermark

  if (!is.null(watermark)) {
    if (isOfType(watermark, "character")) {
      watermark <- asLabel(watermark, watermarkConfiguration$font)
    }
  }

  watermarkConfiguration <- watermark %||% watermarkConfiguration

  # If plot is initialized, addWatermark otherwise update watermark
  if (length(plotObject$layers) == 0) {
    newPlotObject <- addWatermark(newPlotObject,
      label = watermarkConfiguration,
      angle = angle,
      alpha = alpha
    )
    return(newPlotObject)
  }

  # Using initializePlot, watermark will always be as first layer
  dummyPlot <- addWatermark(ggplot2::ggplot(),
    label = watermarkConfiguration,
    angle = angle,
    alpha = alpha
  )

  newPlotObject$layers[[1]] <- dummyPlot$layer[[1]]

  return(newPlotObject)
}

#' @title createWatermarkGrob
#' @param label Character or Label class object corresponding to the watermark text
#' (and its font properties if Label)
#' @param angle Angle in degree from horizontal of the watermark label. Default angle is 30 degrees.
#' @param alpha Transparency of the watermark label.
#' Alpha is a numeric between 0 and 1: 0 label is totally transparent, 1 label is totally opaque.
#' Default alpha is 0.6.
#' @return Watermark background as a ggplot grob object
#' @description
#' createWatermarkGrob creates a ggplot grob based on the label text and its font properties.
#' @export
createWatermarkGrob <- function(label, angle = 30, alpha = 0.6) {
  # Ensure label is a Label class
  label <- asLabel(label)
  watermark <- ggplot2::ggplot() + ggplot2::theme_void() +
    ggplot2::annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = label$text,
      color = label$font$color, fontface = label$font$fontFace, size = label$font$size,
      angle = angle, alpha = alpha
    )
  watermark <- ggplot2::ggplotGrob(watermark)

  return(watermark)
}
