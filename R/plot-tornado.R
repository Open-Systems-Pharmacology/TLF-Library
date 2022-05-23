#' @title plotTornado
#' @description
#' Producing tornado plots
#'
#' @inheritParams addScatter
#' @param y Character values to plot along the `y` axis. Only used instead of `data` if `data` is `NULL`.
#' @param sorted Optional logical value defining if `y` values are sorted by absolute values of `x`.
#' @param colorPalette Optional character values defining a `ggplot2` colorPalette (e.g. `"Spectral"`)
#' @param bar Optional logical value setting tornado plot as bar plot instead of scatter plot.
#' @param dataMapping
#' A `TornadoDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration
#' An optional `TornadoPlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce a tornado plot
#' plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"))
#'
#' # Produce a tornado plot as scatter plot
#' plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"), bar = FALSE)
#'
#' # Produce a tornado plot as is (no sorting)
#' plotTornado(x = c(2, -1, 3), y = c("A", "B", "C"), sorted = FALSE)
#'
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
      x = ifNotNull(x, "x"),
      y = ifNotNull(y, "y"),
      data = data
    )
  }

  dataMapping <- dataMapping %||% TornadoDataMapping$new(data = data)
  dataMapping$sorted <- sorted %||% dataMapping$sorted
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
    mapData[, dataMapping$y] <- stats::reorder(mapData[, dataMapping$y], abs(mapData[, dataMapping$x]))
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
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$alpha, position = 0, aesthetic = "alpha"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$size, position = 0, aesthetic = "size"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$linetype, position = 0, aesthetic = "linetype"),
      position = ggplot2::position_dodge(width = plotConfiguration$dodge)
    )

    # Define shapes and colors based on plotConfiguration$points properties
    fillVariable <- gsub("`", "", mapLabels$fill)
    colorVariable <- gsub("`", "", mapLabels$color)
    fillLength <- length(unique(mapData[, fillVariable]))
    colorLength <- length(unique(mapData[, colorVariable]))

    plotObject <- plotObject +
      ggplot2::scale_fill_manual(values = getAestheticValues(n = fillLength, selectionKey = plotConfiguration$ribbons$fill, aesthetic = "fill")) +
      ggplot2::scale_color_manual(values = getAestheticValues(n = colorLength, selectionKey = plotConfiguration$ribbons$color, aesthetic = "color"))
  }
  if (!plotConfiguration$bar) {
    # For tornado with points, their shape will be taken from the theme properties
    plotObject <- plotObject + ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        color = mapLabels$color,
        shape = mapLabels$shape
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      position = ggplot2::position_dodge(width = plotConfiguration$dodge)
    )

    # Define shapes and colors based on plotConfiguration$points properties
    shapeVariable <- gsub("`", "", mapLabels$shape)
    colorVariable <- gsub("`", "", mapLabels$color)
    shapeLength <- length(unique(mapData[, shapeVariable]))
    colorLength <- length(unique(mapData[, colorVariable]))

    plotObject <- plotObject +
      ggplot2::scale_shape_manual(values = getAestheticValues(n = shapeLength, selectionKey = plotConfiguration$points$shape, aesthetic = "shape")) +
      ggplot2::scale_color_manual(values = getAestheticValues(n = colorLength, selectionKey = plotConfiguration$points$color, aesthetic = "color"))
  }

  # Final plot includes a vertical line in 0
  # And optional color palette otherwise use colors from theme
  if (!isOfLength(dataMapping$lines, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_vline(
        xintercept = dataMapping$lines,
        color = getAestheticValues(n = length(dataMapping$lines), selectionKey = plotConfiguration$lines$color, position = 0, aesthetic = "color"),
        size = getAestheticValues(n = length(dataMapping$lines), selectionKey = plotConfiguration$lines$size, position = 0, aesthetic = "size"),
        linetype = getAestheticValues(n = length(dataMapping$lines), selectionKey = plotConfiguration$lines$linetype, position = 0, aesthetic = "linetype")
      )
  }

  if (!isOfLength(plotConfiguration$colorPalette, 0)) {
    try(suppressMessages(
      plotObject <- plotObject +
        ggplot2::scale_fill_brewer(
          palette = plotConfiguration$colorPalette,
          aesthetics = c("color", "fill")
        )
    ))
  }
  try(suppressMessages(plotObject <- setXAxis(plotObject)))
  try(suppressMessages(plotObject <- setYAxis(plotObject)))
  return(plotObject)
}
