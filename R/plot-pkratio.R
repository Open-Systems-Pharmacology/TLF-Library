#' Create a PK-Ratio plot
#'
#' @param data TODO
#' @param metaData TODO
#' @param dataMapping TODO
#' @param plotConfiguration TODO
#'
#' @return a ggplot graphical object
#' @export
#'
plotPKRatio <- function(data, metaData, dataMapping = NULL, plotConfiguration = NULL) {
  # If no data mapping or plot configuration is input, use default
  configuration <- plotConfiguration %||% PKRatioPlotConfiguration$new()
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))
  stopifnot("PKRatioPlotConfiguration" %in% class(configuration))

  x <- dataMapping$x
  y <- dataMapping$y

  colorGrouping <- getGrouping(data, dataMapping$colorGrouping)
  sizeGrouping <- getGrouping(data, dataMapping$sizeGrouping)
  shapeGrouping <- getGrouping(data, dataMapping$shapeGrouping)

  plotObject <- ggplot()

  # Add Plot Configuration layers and PK Ratios
  plotObject <- plotConfiguration$setWatermark(plotObject)
  plotObject <- plotConfiguration$defineLabels(plotObject, dataMapping)
  plotObject <- plotConfiguration$addRatioLines(plotObject)
  plotObject <- plotObject + geom_point(data = data[, c(dataMapping$x, dataMapping$y, dataMapping$colorGrouping, dataMapping$sizeGrouping, dataMapping$shapeGrouping)], mapping = aes(x = data[, x], y = data[, y], color = colorGrouping, size = sizeGrouping, shape = shapeGrouping))

  return(plotObject)
}
