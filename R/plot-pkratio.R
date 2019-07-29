
#' Create a PK-Ratio plot
#'
#' @param data TODO
#' @param metaData TODO
#' @param dataMapping TODO
#' @param plotConfiguration TODO
#'
#' @return a ggplot object
#' @export
#'
#' @examples
plotPKRatio <- function(data, metaData, dataMapping, plotConfiguration = NULL) {
  configuration <- plotConfiguration %||% PKRatioPlotConfiguration$new()

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))
  stopifnot("PKRatioPlotConfiguration" %in% class(configuration))

  x <- dataMapping$x
  y <- dataMapping$y
  grouping <- dataMapping$grouping

  plot <- ggplot(data, aes(x = data[[x]], y = data[[y]], color = data[[grouping]])) +
    labs(title = configuration$title, x = x, y = y, color = grouping) +
    geom_point()


  plot <- addRatioLines(plot, plotConfiguration)
  plot
}

addRatioLines <- function(plot, plotConfiguration) {
  plot + geom_hline(yintercept = 1, linetype = "solid", color = "black", size = 2) +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 2, linetype = "dashed", color = "red", size = 1) +
    geom_hline(yintercept = 0.75, linetype = "dashed", color = "blue", size = 1) +
    geom_hline(yintercept = 1.55, linetype = "dashed", color = "blue", size = 1)
}
