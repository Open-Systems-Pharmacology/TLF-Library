#' @title plotPKRatio
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' \code{PKRatioDataMapping} class or subclass mapping x and y variables to \code{data} variable names.
#' \code{dataMapping} provides also the values of the PK Ratio limits plotted as horizontal lines.
#' This parameter is optional: the \code{tlf} library provides a smart mapping if only \code{data} is provided
#' and default values of the PK Ratio limits.
#' @param plotConfiguration
#' \code{PKRatioPlotConfiguration} class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @param plotObject \code{ggplot} graphical object to which the PK Ratio plot layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add PK Ratio plot layers to a \code{ggplot} graphical object.
#' PK Ratio limits are plotted as horizontal lines.
#' PK Ratios are plotted as a scatter plot.
#' @return A \code{ggplot} graphical object
#' @export
plotPKRatio <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        plotObject = NULL) {
  dataMapping <- dataMapping %||% PKRatioDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% PKRatioPlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, PKRatioDataMapping)
  validateIsOfType(plotConfiguration, PKRatioPlotConfiguration)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  for (lineIndex in seq_along(dataMapping$pkRatioValues)) {
    plotObject <- addLine(
      y = dataMapping$pkRatioValues[[lineIndex]],
      caption = paste0("pkRatioLine", lineIndex),
      plotObject = plotObject
    )
  }
  plotObject <- setLegendCaption(plotObject, plotConfiguration$pkRatioCaption)
  plotObject <- addScatter(data = data, dataMapping = dataMapping, plotObject = plotObject)
  return(plotObject)
}
