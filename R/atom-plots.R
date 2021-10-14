# Atom plots

#' @title initializePlot
#' @param plotConfiguration
#' `PlotConfiguration` objecct defining labels, grid, background and watermark
#' This parameter is optional: the `tlf` library provides a default configuration according to the current theme
#' @description
#' Initialize a `ggplot` object and set the labels, grid, background and watermark
#' @return A `ggplot` graphical object
#' @examples
#' # Initialize an empty plot
#' p <- initializePlot()
#'
#' # Implement a customized configuration using PlotConfiguration
#' config <- PlotConfiguration$new(title = "My Plot", xlabel = "x variable", ylabel = "y variable")
#' p <- initializePlot(config)
#' @export
initializePlot <- function(plotConfiguration = NULL) {
  validateIsOfType(plotConfiguration, "PlotConfiguration", nullAllowed = TRUE)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new()

  plotObject <- ggplot2::ggplot()
  plotObject$plotConfiguration <- plotConfiguration

  plotObject <- setWatermark(plotObject)
  plotObject <- setBackgroundPlotArea(plotObject)
  plotObject <- setBackgroundPanelArea(plotObject)
  plotObject <- setXGrid(plotObject)
  plotObject <- setYGrid(plotObject)
  plotObject <- setPlotLabels(plotObject)

  return(plotObject)
}

#' @title addScatter
#' @param data data.frame containing the scatter points to be plotted
#' @param metaData list of information on `data` such as `dimension` and `unit` of their variables
#' @param x Mapping for x values.
#' If `data` is NULL or not input, `x` numeric values will be used as is for the plot.
#' @param y Mapping for y values.
#' If `data` is NULL or not input, `y` numeric values will be used as is for the plot.
#' @param dataMapping `XYGDataMapping` class or subclass object
#' mapping x, y and aesthetic variables to the variable names of `data`.
#' @param caption vector of character strings defining the legend captions.
#' This parameter is optional: default value `NULL` creates caption labels "data 1", "data 2" ...
#' @param color vector of character strings defining the color of the scatter points.
#' This parameter is optional: default value `NULL` will choose colors according to the current theme.
#' @param shape vector of character strings or numerical defining the shapes of the scatter points.
#' This parameter is optional: default value `NULL` will choose shapes according to the current theme.
#' @param size vector of numerical defining the sizes of the scatter points.
#' This parameter is optional: default value `NULL` will choose sizes according to the current theme.
#' @param linetype vector of character strings defining the linetype linking the scatter points.
#' This parameter is optional: default value `NULL` won't provide lines.
#' @param plotConfiguration `PlotConfiguration` object defining the labels, axes, background and legend properties of the plot.
#' @param plotObject `ggplot` graphical object to which the line layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a scatter plot layer to a `ggplot` graphical object.
#' Use optional argument `caption` to set legend caption.
#' Since `ggplot` manage aesthetic properties across all layers,
#' aesthetic properties defined in `plotConfiguration` will apply across all layers.
#' @return A `ggplot` graphical object
#' @examples
#' # Add scatter using x and y
#' p <- addScatter(x = c(1, 2, 1, 2, 3), y = c(5, 0, 2, 3, 4))
#'
#' # Add a custom scatter
#' time <- seq(0, 30, 0.1)
#' customScatterData <- data.frame(x = time, y = cos(time))
#'
#' p <- addScatter(
#'   data = customScatterData,
#'   dataMapping = XYGDataMapping$new(x = "x", y = "y")
#' )
#'
#' # Or for simple cases a smart mapping will get directly x and y from data
#' p <- addScatter(data = customScatterData)
#'
#' # Add a scatter with caption
#' p <- addScatter(data = customScatterData, caption = "My scatter plot")
#'
#' # Add a scatter with specific properties
#' p <- addScatter(data = customScatterData, color = "blue", shape = 19, size = 2, caption = "My data")
#'
#' # Add a scatter with specific properties
#' pp <- addScatter(x = c(0, 1), y = c(1, 0), color = "red", shape = 20, size = 3, plotObject = p)
#' @export
addScatter <- function(data = NULL,
                       metaData = NULL,
                       x = NULL,
                       y = NULL,
                       caption = NULL,
                       color = NULL,
                       shape = NULL,
                       size = NULL,
                       linetype = NULL,
                       dataMapping = NULL,
                       plotConfiguration = NULL,
                       plotObject = NULL) {
  validateIsOfType(dataMapping, XYGDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, PlotConfiguration, nullAllowed = TRUE)

  # If data is not input, creates data from x and y inputs
  if (isOfLength(data, 0)) {
    validateIsSameLength(x, y)
    data <- as.data.frame(cbind(x = x, y = y))
    dataMapping <- dataMapping %||% XYGDataMapping$new(x = ifnotnull(x, "x"), y = ifnotnull(y, "y"), data = data)
  }
  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% XYGDataMapping$new(x = x, y = y, data = data)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)
  # Update plotConfiguration if user defined aesthetics
  eval(parseVariableToObject("plotConfiguration$points", c("color", "shape", "linetype", "size"), keepIfNull = TRUE))

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, nor x or y, return plotObject
  if (any(isOfLength(dataMapping$x, 0), isOfLength(dataMapping$y, 0))) {
    warning("No mapping found for x nor y, scatter layer was not added")
    return(plotObject)
  }

  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels

  plotObject <- plotObject +
    ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y, shape = "legendLabels", color = "legendLabels", size = "legendLabels"),
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    ggplot2::geom_path(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$y, ymax = mapLabels$y, fill = "legendLabels"),
      # alpha is 0 so that line can be seen in legend
      alpha = 0,
      show.legend = TRUE,
      na.rm = TRUE
    )

  # Prepare data for merging previous and current legend
  newLabels <- levels(factor(mapData$legendLabels))
  # Sample LegendType properties based Theme if not input
  try(plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    aestheticSelections = plotConfiguration$points
  ))
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @title addLine
#' @param data data.frame containing the line endpoints to be plotted
#' @param metaData list of information on `data` such as `dimension` and `unit` of their variables
#' @param x Mapping for x values.
#' If `data` is NULL or not input, `x` numeric values will be used as is for the plot.
#' In case there is no `y` mapping, `x` are the x-values of vertical lines.
#' @param y Mapping for y values.
#' If `data` is NULL or not input, `y` numeric values will be used as is for the plot.
#' In case there is no `x` mapping, `y` are the y-values of horizontal lines.
#' @param dataMapping `XYGDataMapping` class or subclass
#' mapping x, y and aesthetic variables to the variable names of `data`.
#' @param caption vector of character strings defining the legend captions.
#' This parameter is optional: default value is NULL.
#' @param color vector of character strings defining the color of the scatter points.
#' This parameter is optional: default value `NULL` will choose colors according to the current theme.
#' @param shape vector of character strings or numerical defining the shapes of the scatter points.
#' This parameter is optional: default value `NULL` will choose shapes according to the current theme.
#' @param size vector of numerical defining the sizes of the scatter points.
#' This parameter is optional: default value `NULL` will choose sizes according to the current theme.
#' @param linetype vector of character strings defining the linetype linking the scatter points.
#' This parameter is optional: default value `NULL` won't provide lines.
#' @param plotConfiguration `PlotConfiguration` object defining the label and background properties of the plot.
#' @param plotObject `ggplot` graphical object to which the line layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a line layer to a `ggplot` graphical object.
#' Use optional argument `caption` to set legend caption.
#' Since `ggplot` manage aesthetic properties across all layers,
#' aesthetic properties defined in `plotConfiguration` will apply across all layers.
#' @return A `ggplot` graphical object
#' @examples
#' # Add vertical line at x = 2
#' addLine(x = 2)
#'
#' # Add vertical line at x = 2 to a previous plot
#' p <- initializePlot() +
#'   ggplot2::geom_point(
#'     data = data.frame(x = c(1, 2, 3), y = c(5, 6, 4)),
#'     mapping = ggplot2::aes_string(x = "x", y = "y")
#'   )
#' p <- addLine(x = 2, plotObject = p)
#'
#' # Add horizontal line at y = 5 to a previous plot
#' p <- addLine(y = 5, plotObject = p)
#'
#' # Add a custom line
#' time <- seq(0, 30, 0.01)
#' customLineData <- data.frame(x = time, y = cos(time))
#'
#' p <- addLine(
#'   data = customLineData,
#'   dataMapping = XYGDataMapping$new(x = "x", y = "y")
#' )
#'
#' # Or for simple cases a smart mapping will get directly x and y from data
#' p <- addLine(data = customLineData)
#'
#' # Add a line with caption
#' p <- addLine(data = customLineData, caption = "My line")
#' @export
addLine <- function(data = NULL,
                    metaData = NULL,
                    x = NULL,
                    y = NULL,
                    caption = NULL,
                    color = NULL,
                    shape = NULL,
                    size = NULL,
                    linetype = NULL,
                    dataMapping = NULL,
                    plotConfiguration = NULL,
                    plotObject = NULL) {
  validateIsOfType(dataMapping, XYGDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, PlotConfiguration, nullAllowed = TRUE)

  # If data is not input,  creates new data and its mapping from x and y input
  if (isOfLength(data, 0)) {
    data <- as.data.frame(cbind(x = x, y = y))

    dataMapping <- dataMapping %||% XYGDataMapping$new(x = ifnotnull(x, "x"), y = ifnotnull(y, "y"), data = data)
  }

  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% XYGDataMapping$new(x = x, y = y, data = data)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)
  # Update plotConfiguration if user defined aesthetics
  eval(parseVariableToObject("plotConfiguration$lines", c("color", "shape", "linetype", "size"), keepIfNull = TRUE))

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (all(isOfLength(dataMapping$x, 0), isOfLength(dataMapping$y, 0))) {
    warning("No mapping found for both x and y, line layer was not added")
    return(plotObject)
  }

  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels

  # y-intercept
  # geom_blank is used to fill the missing aes properties
  # This prevents messing up the legend
  if (isOfLength(dataMapping$x, 0) && !isOfLength(dataMapping$y, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_hline(
        data = mapData,
        mapping = ggplot2::aes_string(yintercept = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = ggplot2::aes_string(
          shape = "legendLabels",
          fill = "legendLabels"
        ),
        show.legend = TRUE
      )
  }

  # x-intercept
  if (isOfLength(dataMapping$y, 0) && !isOfLength(dataMapping$x, 0)) {
    plotObject <- plotObject +
      ggplot2::geom_vline(
        data = mapData,
        mapping = ggplot2::aes_string(xintercept = mapLabels$x, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = ggplot2::aes_string(
          shape = "legendLabels",
          fill = "legendLabels"
        ),
        show.legend = TRUE
      )
  }

  # Case of a line defined by x and y
  # geom_path is used instead of geom_line,
  # consequently values are connected by their order of appearance and not according to x values
  if (all(!isOfLength(dataMapping$x, 0), !isOfLength(dataMapping$y, 0))) {
    plotObject <- plotObject +
      ggplot2::geom_point(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y, shape = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE,
        na.rm = TRUE
      ) +
      ggplot2::geom_path(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE,
        na.rm = TRUE
      ) +
      ggplot2::geom_ribbon(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$y, ymax = mapLabels$y, fill = "legendLabels"),
        # alpha is 0 so that line can be seen in legend
        alpha = 0,
        show.legend = TRUE,
        na.rm = TRUE
      )
  }

  # Prepare data for merging previous and current legend
  newLabels <- levels(factor(mapData$legendLabels))
  # Sample LegendType properties based Theme if not input
  try(plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    aestheticSelections = plotConfiguration$lines
  ))
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @title addRibbon
#' @param data data.frame containing the ribbon endpoints to be plotted.
#' @param metaData list of information on `data` such as `dimension` and `unit` of their variables
#' @param x Mapping for x values.
#' If `data` is NULL or not input, `x` numeric values will be used as is for the plot.
#' @param ymin Mapping for ymin values.
#' If `data` is NULL or not input, `ymin` numeric values will be used as is for the plot.
#' In case there is no `x` mapping, `ymin` are the y-values of horizontal lines.
#' In case there is no `ymax` mapping, a value of 0 is assumed for `ymax`.
#' @param ymax Mapping for ymax values.
#' If `data` is NULL or not input, `ymax` numeric values will be used as is for the plot.
#' In case there is no `x` mapping, `ymax` are the y-values of horizontal lines.
#' In case there is no `ymin` mapping, a value of 0 is assumed for `ymin`.
#' @param dataMapping `XYGDataMapping` class or subclass
#' mapping x, y and aesthetic variables to the variable names of `data`.
#' @param caption vector of character strings defining the legend captions.
#' This parameter is optional: default value is NULL.
#' @param fill vector of character strings defining the color of the scatter points.
#' This parameter is optional: default value `NULL` will choose colors according to the current theme.
#' @param color vector of character strings defining the color of the scatter points.
#' This parameter is optional: default value `NULL` will choose colors according to the current theme.
#' @param size vector of numerical defining the sizes of the scatter points.
#' This parameter is optional: default value `NULL` will choose sizes according to the current theme.
#' @param linetype vector of character strings defining the linetype linking the scatter points.
#' This parameter is optional: default value `NULL` won't provide lines.
#' @param alpha transparency of the ribbon.
#' Numeric value between 0 and 1. Value of 0, the plot is transparent. Value of 1, the plot is opaque.
#' Default value for `alpha` is 0.8.
#' @param plotConfiguration `PlotConfiguration` object defining the label and background properties of the plot.
#' @param plotObject `ggplot` graphical object to which the line layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a ribbon layer to a `ggplot` graphical object.
#' Use optional argument `caption` to set legend caption.
#' Since `ggplot` manage aesthetic properties across all layers,
#' aesthetic properties defined in `plotConfiguration` will apply across all layers.
#' @return A `ggplot` graphical object
#' @examples
#' # Add a horizontal ribbon to a previous plot
#' p <- addRibbon(ymin = -5, ymax = 5)
#'
#' # Add a horizontal ribbon to a previous plot
#' p <- addLine(x = c(1, 2), y = c(0, 0))
#' p <- addRibbon(ymin = -5, ymax = 5, plotObject = p)
#'
#' # Add a custom ribbon
#' time <- seq(0, 30, 0.01)
#' customData <- data.frame(x = time, y = cos(time), ymin = cos(time) - 0.5, ymax = cos(time) + 0.5)
#'
#' p <- addRibbon(
#'   data = customData,
#'   dataMapping = RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax")
#' )
#'
#' # Or for simple cases a smart mapping will get directly x and y from data
#' p <- addRibbon(data = customData)
#'
#' # Add a ribbon with caption
#' pr <- addRibbon(data = customData, caption = "My plot with ribbon")
#'
#' # Add a ribbon with different transparency
#' p <- addRibbon(data = customData, alpha = 1)
#'
#' # Add a scatter to the ribbon
#' pScatter <- addScatter(data = customData, caption = "My plot with ribbon", plotObject = pr)
#'
#' # Add a scatter to the ribbon
#' pLine <- addLine(data = customData, caption = "My plot with ribbon", plotObject = pr)
#' @export
addRibbon <- function(data = NULL,
                      metaData = NULL,
                      x = NULL,
                      ymin = NULL,
                      ymax = NULL,
                      caption = NULL,
                      fill = NULL,
                      color = NULL,
                      size = NULL,
                      linetype = NULL,
                      alpha = 0.8,
                      dataMapping = NULL,
                      plotConfiguration = NULL,
                      plotObject = NULL) {
  validateIsOfType(dataMapping, RangeDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, PlotConfiguration, nullAllowed = TRUE)

  # If data is not input, creates data and its mapping from x, ymin and ymax input
  if (isOfLength(data, 0)) {
    data <- as.data.frame(cbind(x = x, ymin = ymin %||% 0, ymax = ymax %||% 0))

    # y-intercept ribbon
    if (isOfLength(x, 0)) {
      # Redefine data.frame for y-intercept ribbon
      data <- rbind.data.frame(cbind.data.frame(x = -Inf, data), cbind.data.frame(x = Inf, data))
    }
    dataMapping <- dataMapping %||% RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax", data = data)
  }
  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% RangeDataMapping$new(x = x, ymin = ymin, ymax = ymax, data = data)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)
  # Update plotConfiguration if user defined aesthetics
  eval(parseVariableToObject("plotConfiguration$ribbons", c("color", "fill", "linetype", "size", "alpha"), keepIfNull = TRUE))

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (all(isOfLength(dataMapping$x, 0), isOfLength(dataMapping$ymin, 0), isOfLength(dataMapping$ymax, 0))) {
    warning("No mapping found for x, ymin and ymax, ribbon layer was not added")
    return(plotObject)
  }

  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels

  plotObject <- plotObject +
    ggplot2::geom_ribbon(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$ymax, fill = "legendLabels"),
      alpha = alpha,
      show.legend = TRUE,
      na.rm = TRUE
    ) +
    ggplot2::geom_blank(
      data = mapData,
      mapping = aes_string(shape = "legendLabels", color = "legendLabels", size = "legendLabels", linetype = "legendLabels")
    )

  # Prepare data for merging previous and current legend
  newLabels <- levels(factor(mapData$legendLabels))
  # Sample LegendType properties based Theme if not input
  try(plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    aestheticSelections = plotConfiguration$ribbons
  ))
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @title addErrorbar
#' @param data data.frame containing the errorbar endpoints to be plotted
#' @param metaData list of information on `data` such as `dimension` and `unit` of their variables
#' @param x Mapping for x values.
#' If `data` is NULL or not input, `x` numeric values will be used as is for the plot.
#' @param ymin Mapping for ymin values.
#' If `data` is NULL or not input, `ymin` numeric values will be used as is for the plot.
#' @param ymax Mapping for ymin values.
#' If `data` is NULL or not input, `ymax` numeric values will be used as is for the plot.
#' @param caption vector of character strings defining the legend captions.
#' This parameter is optional: default value is NULL.
#' @param color vector of character strings defining the color of the scatter points.
#' This parameter is optional: default value `NULL` will choose colors according to the current theme.
#' @param size vector of numerical defining the sizes of the scatter points.
#' This parameter is optional: default value `NULL` will choose sizes according to the current theme.
#' @param linetype vector of character strings defining the linetype linking the scatter points.
#' This parameter is optional: default value `NULL` won't provide lines.
#' @param includeCap logical setting if error bars include caps at their ends.
#' @param dataMapping `RangeDataMapping` class or subclass
#' mapping x, ymin, ymax and aesthetic variables to the variable names of `data`.
#' @param plotConfiguration `PlotConfiguration` object defining the label and background properties of the plot.
#' @param plotObject `ggplot` graphical object to which the line layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a errorbar plot layer to a `ggplot` graphical object.
#' Use optional argument `caption` to set legend caption.
#' Since `ggplot` manage aesthetic properties across all layers,
#' aesthetic properties defined in `plotConfiguration` will apply across all layers.
#' If caption is the same as a previous scatter plot layer, the legend will merge their caption and aesthetic properties
#' @return A `ggplot` graphical object
#' @export
addErrorbar <- function(data = NULL,
                        metaData = NULL,
                        x = NULL,
                        ymin = NULL,
                        ymax = NULL,
                        caption = NULL,
                        color = NULL,
                        size = NULL,
                        linetype = NULL,
                        includeCap = FALSE,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        plotObject = NULL) {
  validateIsOfType(dataMapping, c("RangeDataMapping", "ObservedDataMapping"), nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, PlotConfiguration, nullAllowed = TRUE)
  validateIsLogical(includeCap)
  # validateIsIncluded(barLinetype, Linetypes, nullAllowed = TRUE)

  # If data is not input, creates data and its mapping from x, ymin and ymax input
  if (isOfLength(data, 0)) {
    data <- as.data.frame(cbind(x = x, ymin = ymin %||% 0, ymax = ymax %||% 0))
    dataMapping <- dataMapping %||% RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax", data = data)
  }
  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% RangeDataMapping$new(x = x, ymin = ymin, ymax = ymax, data = data)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)
  # Update plotConfiguration if user defined aesthetics
  eval(parseVariableToObject("plotConfiguration$errorbars", c("color", "linetype", "size"), keepIfNull = TRUE))

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (all(isOfLength(dataMapping$x, 0), isOfLength(dataMapping$ymin, 0), isOfLength(dataMapping$ymax, 0))) {
    warning("No mapping found for x, ymin and ymax, error bar layer was not added")
    return(plotObject)
  }

  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)
  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels
  legendLength <- length(unique(mapData$legendLabels))

  # Option caps allows to add an horizontal bar at the edges of the error bars
  if (includeCap) {
    plotObject <- plotObject +
      ggplot2::geom_errorbar(
        data = mapData,
        mapping = ggplot2::aes_string(
          x = mapLabels$x,
          ymin = mapLabels$ymin,
          ymax = mapLabels$ymax,
          color = "legendLabels",
        ),
        na.rm = TRUE,
        size = size %||% getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, aesthetic = "size"),
        linetype = linetype %||% getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype")
      )
  }
  if (!includeCap) {
    plotObject <- plotObject +
      ggplot2::geom_linerange(
        data = mapData,
        mapping = ggplot2::aes_string(
          x = mapLabels$x,
          ymin = mapLabels$ymin,
          ymax = mapLabels$ymax,
          color = "legendLabels",
        ),
        na.rm = TRUE,
        size = size %||% getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, aesthetic = "size"),
        linetype = linetype %||% getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype")
      )
  }
  plotObject <- plotObject +
    ggplot2::geom_ribbon(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        ymin = mapLabels$ymin,
        ymax = mapLabels$ymax,
        fill = "legendLabels",
      ),
      alpha = 0,
      show.legend = TRUE,
      na.rm = TRUE
    )
  # Add blank lines, points and ribbon to the plot
  plotObject <- plotObject +
    ggplot2::geom_blank(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$ymax,
        shape = "legendLabels",
        color = "legendLabels",
        fill = "legendLabels",
        size = "legendLabels",
        linetype = "legendLabels"
      )
    )

  # Prepare data for merging previous and current legend
  newLabels <- levels(factor(mapData$legendLabels))
  # Sample LegendType properties based Theme if not input
  try(plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    aestheticSelections = plotConfiguration$errorbars
  ))
  # Try is used to prevent crashes in the final plot due to ggplot2 peculiarities regarding scale functions
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @keywords internal
getlegendLabelsCaption <- function(plotObject) {
  legendLabelsCaptionCount <- which(paste0("data ", seq(1, 100)) %in% plotObject$plotConfiguration$legend$caption$label)
  if (length(legendLabelsCaptionCount) == 0) {
    legendLabelsCaptionCount <- 0
  }
  return(paste0("data ", max(legendLabelsCaptionCount) + 1))
}
