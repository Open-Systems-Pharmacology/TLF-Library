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
  validateIsOfType(data, "data.frame")
  dataMapping <- dataMapping %||% PKRatioDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% PKRatioPlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, "PKRatioDataMapping")
  validateIsOfType(plotConfiguration, "PKRatioPlotConfiguration")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("PK ratio plot"))
    return(plotObject)
  }

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  # Include horizontal lines
  for (lineIndex in seq_along(dataMapping$lines)) {
    # position correspond to the number of layer lines already added
    plotObject <- plotObject +
      ggplot2::geom_hline(
        yintercept = dataMapping$lines[[lineIndex]],
        color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex - 1, aesthetic = "color"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex - 1, aesthetic = "linetype"),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex - 1, aesthetic = "size")
      )
  }

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_linerange(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          ymin = "ymin",
          ymax = "ymax",
          color = mapLabels$color
        ),
        # Error bar size uses a ratio of 1/4 to match with point size
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype"),
        show.legend = TRUE
      )
  }
  plotObject <- plotObject +
    ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        color = mapLabels$color,
        shape = mapLabels$shape
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      show.legend = TRUE
    )

  # Define shapes and colors based on plotConfiguration$points properties
  shapeVariable <- gsub("`", "", mapLabels$shape)
  colorVariable <- gsub("`", "", mapLabels$color)
  shapeLength <- length(unique(mapData[, shapeVariable]))
  colorLength <- length(unique(mapData[, colorVariable]))

  plotObject <- plotObject +
    ggplot2::scale_shape_manual(values = getAestheticValues(n = shapeLength, selectionKey = plotConfiguration$points$shape, aesthetic = "shape")) +
    ggplot2::scale_color_manual(values = getAestheticValues(n = colorLength, selectionKey = plotConfiguration$points$color, aesthetic = "color"))

  # If variable is legendLabel, remove it from legend
  if (isIncluded(shapeVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(shape = FALSE)
  }
  if (isIncluded(colorVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(color = FALSE)
  }
  # Try is used to prevent crashes in the final plot due to ggplot2 peculiarities regarding scale functions
  try(suppressMessages(plotObject <- setXAxis(plotObject)))
  try(suppressMessages(plotObject <- setYAxis(plotObject)))
  return(plotObject)
}
