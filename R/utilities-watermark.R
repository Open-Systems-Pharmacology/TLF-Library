#' @title setWatermark
#' @param plotHandle ggplot object
#' @param label to be written in watermark (class Label and character allowed)
#' @param angle (optional argument) angle of the watermark
#' @param alpha (optional argument) transparency of the watermark
#' @return plotHandle ggplot object with watermark background as a grob
#' @description
#' setWatermark set watermark as defined by label
#' @import  ggplot2
#' @export
setWatermark <- function(plotHandle, label, angle = 30, alpha = 0.4) {
  label <- asLabel(label)

  watermark <- createWatermarkGrob(label = label, angle = angle, alpha = alpha)

  plotHandle <- plotHandle + ggplot2::annotation_custom(grob = watermark, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf)

  return(plotHandle)
}

#' @title createWatermarkGrob
#' @param label Label class of Watermark
#' @param angle orientation of the watermark
#' @param alpha (optional argument) transparency of the watermark
#' @return watermark Background as a grob for ggplot objects
#' @description
#' createWatermarkGrob use a label to define a Background Watermark as a grob
#' @export
createWatermarkGrob <- function(label, angle = 30, alpha = 0.4) {
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
