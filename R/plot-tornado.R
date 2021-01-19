#' @title plotTornado
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param x Mapping for x (values of tornado plot)
#' If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.
#' @param y Mapping for y (labels of tornado plot)
#' If \code{data} is NULL or not input, \code{y} character values will be used as is for the plot.
#' @param sorted logical indicating if values should be sorted if \code{dataMapping} is not input.
#' By default, values are sorted by their absolute values
#' @param colorPalette Define a `ggplot2` colorPalette (e.g `colorPalette`="Spectral")
#' @param bar logical setting tornado as bar plot
#' @param dataMapping
#' \code{TornadoDataMapping} class or subclass mapping x and y variables to \code{data} variable names.
#' \code{dataMapping} provides also the values of the PK Ratio limits plotted as horizontal lines.
#' This parameter is optional: the \code{tlf} library provides a smart mapping if only \code{data} is provided
#' and default values of the PK Ratio limits.
#' @param plotConfiguration
#' \code{TornadoPlotConfiguration} class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @param plotObject \code{ggplot} graphical object to which the tornado plot layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add tornado plot layers to a \code{ggplot} graphical object.
#' Tornado limits are plotted as vertical lines.
#' Tornado values are plotted as horizontal bars or point according to option `bar`
#' @return A \code{ggplot} graphical object
#' @export
plotTornado <- function(data = NULL,
                        metaData = NULL,
                        x = NULL,
                        y = NULL,
                        sorted = NULL,
                        colorPalette = NULL,
                        bar = TRUE,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        plotObject = NULL) {
  validateIsOfType(data, "data.frame", nullAllowed = TRUE)
  validateIsString(colorPalette, nullAllowed = TRUE)
  validateIsLogical(sorted, nullAllowed = TRUE)
  validateIsLogical(bar)

  if (is.null(data)) {
    validateIsNumeric(x)
    y <- y %||% rep("", length(x))
    validateIsSameLength(x, y)
    data <- data.frame(x = x, y = y)

    dataMapping <- dataMapping %||% TornadoDataMapping$new(
      sorted = sorted,
      x = ifnotnull(x, "x"),
      y = ifnotnull(y, "y"),
      data = data
    )
  }

  dataMapping <- dataMapping %||% TornadoDataMapping$new(sorted = sorted, data = data)
  plotConfiguration <- plotConfiguration %||% TornadoPlotConfiguration$new(
    bar = bar,
    colorPalette = colorPalette,
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, "TornadoDataMapping")
  validateIsOfType(plotConfiguration, "TornadoPlotConfiguration")

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  # Option sorting the values, which put the wider spread at the top and smaller at the bottom
  # An additional options can be added to change the type of sort for later versions
  # (e.g. increasing/decreasing absolute values or actual values...)
  if (dataMapping$sorted) {
    mapData[, dataMapping$y] <- reorder(mapData[, dataMapping$y], abs(mapData[, dataMapping$x]))
  }

  # If tornado is a bar plot, plot configuration option "bar" is TRUE.
  # Otherwise, the plot will use points instead
  if (plotConfiguration$bar) {
    plotObject <- plotObject + ggplot2::geom_col(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        fill = mapLabels$fill,
        color = mapLabels$color
      ),
      alpha = plotConfiguration$theme$aesProperties$alpha[1],
      size = plotConfiguration$theme$aesProperties$size[1],
      linetype = plotConfiguration$theme$aesProperties$linetype[1],
      position = ggplot2::position_dodge(width = plotConfiguration$dodge)
    )
  }
  if (!plotConfiguration$bar) {
    plotObject <- plotObject + ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        color = mapLabels$color,
        shape = mapLabels$shape
      ),
      size = plotConfiguration$theme$aesProperties$size[1],
      position = ggplot2::position_dodge(width = plotConfiguration$dodge)
    )
  }

  # Final plot includes a vertical line in 0
  # And optional color palette
  plotObject <- plotObject + 
      ggplot2::geom_vline(
        xintercept = dataMapping$tornadoValues,
        color = plotConfiguration$theme$aesProperties$color[1],
        size = plotConfiguration$theme$aesProperties$size[1],
        linetype = plotConfiguration$theme$aesProperties$linetype[1]
      ) + ifnotnull(plotConfiguration$colorPalette, 
                    ggplot2::scale_fill_brewer(palette = plotConfiguration$colorPalette, 
                                               aesthetics = c("color", "fill")))
  return(plotObject)
}
