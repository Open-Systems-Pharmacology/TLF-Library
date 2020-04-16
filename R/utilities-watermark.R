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

  # Clone tlfConfiguration into a new plot object
  # Prevents update of R6 class being spread to plotObject
  newPlotObject <- plotObject
  newPlotObject$tlfConfiguration <- plotObject$tlfConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$tlfConfiguration$background$watermark
  watermarkConfiguration <- newPlotObject$tlfConfiguration$background$watermark

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
