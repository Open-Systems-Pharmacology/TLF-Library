#' @title plotPKRatio
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' `PKRatioDataMapping` class or subclass mapping x and y variables to `data` variable names.
#' `dataMapping` provides also the values of the PK Ratio limits plotted as horizontal lines.
#' This parameter is optional: the `tlf` library provides a smart mapping if only `data` is provided
#' and default values of the PK Ratio limits.
#' @param plotConfiguration
#' `PKRatioPlotConfiguration` class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the `tlf` library provides a default configuration according to the current theme
#' @param plotObject `ggplot` graphical object to which the PK Ratio plot layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add PK Ratio plot layers to a `ggplot` graphical object.
#' PK Ratio limits are plotted as horizontal lines.
#' PK Ratios are plotted as a scatter plot.
#' @return A `ggplot` graphical object
#' @export
plotPKRatio <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        plotObject = NULL) {
  eval(parseCheckPlotInputs("PKRatio"))
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Include horizontal lines
  for (lineIndex in seq_along(dataMapping$lines)) {
    # position correspond to the number of layer lines already added
    eval(parseAddLineLayer("horizontal", dataMapping$lines[[lineIndex]], lineIndex - 1))
  }

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    eval(parseAddUncertaintyLayer())
  }

  eval(parseAddScatterLayer())
  # Define shapes and colors based on plotConfiguration$points properties
  eval(parseUpdateAestheticProperty(AestheticProperties$color, "points"))
  eval(parseUpdateAestheticProperty(AestheticProperties$shape, "points"))
  plotObject <- setLegendPosition(plotObject)
  plotObject <- setLegendFont(plotObject)
  eval(parseUpdateAxes())
  return(plotObject)
}
