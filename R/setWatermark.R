#' @title setWatermark
#' @param plotHandle ggplot object
#' @param label to be written in watermark (class Label and character allowed)
#' @param angle (optional argument) angle of the watermark
#' @return plotHandle ggplot object with watermark background as a grob
#' @description
#' setWatermark set watermark as defined by label
#' @import  ggplot2
#' @export
setWatermark <- function(plotHandle, label, angle = 30) {
  stopifnot("Label" %in% class(label))

  watermark <- createWatermarkGrob(label, angle)

  plotHandle <- plotHandle + ggplot2::annotation_custom(grob = watermark, xmin = -Inf, ymin = -Inf, xmax = Inf, ymax = Inf)

  return(plotHandle)
}

#' @title createWatermarkGrob
#' @param label Label class of Watermark
#' @param angle orientation of the watermark
#' @return watermark Background as a grob for ggplot objects
#' @description
#' createWatermarkGrob use a label to define a Background Watermark as a grob
#' @export
createWatermarkGrob <- function(label, angle = 30) {
  watermark <- ggplot2::ggplot() + ggplot2::theme_void() +
    annotate(
      geom = "text",
      x = 0,
      y = 0,
      label = label$text,
      color = label$font$color, fontface = label$font$fontface, size = label$font$size,
      angle = angle
    )
  watermark <- ggplot2::ggplotGrob(watermark)

  return(watermark)
}
