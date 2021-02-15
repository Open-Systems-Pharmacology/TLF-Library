# Atom plots

#' @title initializePlot
#' @param plotConfiguration
#' \code{PlotConfiguration} objecct defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @description
#' Initialize a \code{ggplot} object and set the labels, grid, background and watermark
#' @return A \code{ggplot} graphical object
#' @export
#' @examples
#' # Initialize an empty plot
#' p <- initializePlot()
#'
#' # Use a predifined theme
#' useTheme(tlfTheme)
#' p <- initializePlot()
#'
#' # Implement a customized configuration using PlotConfiguration
#' config <- PlotConfiguration$new(title = "My Plot", xlabel = "x variable", ylabel = "y variable")
#' p <- initializePlot(config)
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
#' @param metaData list of information on \code{data} such as \code{dimension} and \code{unit} of their variables
#' @param x Mapping for x values.
#' If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.
#' @param y Mapping for y values.
#' If \code{data} is NULL or not input, \code{y} numeric values will be used as is for the plot.
#' @param dataMapping \code{XYGDataMapping} class or subclass object
#' mapping x, y and aesthetic variables to the variable names of \code{data}.
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
#' @param plotConfiguration \code{PlotConfiguration} object defining the labels, axes, background and legend properties of the plot.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a scatter plot layer to a \code{ggplot} graphical object.
#' Use optional argument \code{caption} to set legend caption.
#' Since \code{ggplot} manage aesthetic properties across all layers,
#' aesthetic properties defined in \code{plotConfiguration} will apply across all layers.
#' @return A \code{ggplot} graphical object
#' @export
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

  # If data is not input
  # Create new data and its mapping from x and y input
  if (is.null(data)) {
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

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, nor x or y, return plotObject
  if (is.null(dataMapping$x) || is.null(dataMapping$y)) {
    warning("No mapping found for x or y, scatter layer was not added")
    return(plotObject)
  }

  # Get transformed data from mapping and convert labels into characters usable by aes_string
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
      show.legend = TRUE
    ) +
    ggplot2::geom_line(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, y = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
      show.legend = TRUE
    ) +
    ggplot2::geom_ribbon(
      data = mapData,
      mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$y, ymax = mapLabels$y, fill = "legendLabels"),
      show.legend = TRUE
    )

  # Prepare data for merging previous and current legend
  # TO DO: base modulo of newLegendProperty on theme aesProperties lengths
  newLabels <- levels(factor(mapData$legendLabels))
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newLegendProperty <- seq(legendLength + 1, legendLength + length(newLabels)) - 1 %% 6 + 1

  # Sample LegendType properties based tlfTheme if not input
  plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    color = color %||% tlfEnv$currentTheme$aesProperties$color[newLegendProperty],
    shape = shape %||% tlfEnv$currentTheme$aesProperties$shape[newLegendProperty],
    size = size %||% rep(1, length(newLabels)),
    linetype = linetype %||% rep("blank", length(newLabels)),
    fill = rep(NA, length(newLabels))
  )
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$xAxis$setPlotAxis(plotObject)))
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$yAxis$setPlotAxis(plotObject)))
  return(plotObject)
}

#' @title addLine
#' @param data data.frame containing the line endpoints to be plotted
#' @param metaData list of information on \code{data} such as \code{dimension} and \code{unit} of their variables
#' @param x Mapping for x values.
#' If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.
#' In case there is no \code{y} mapping, \code{x} are the x-values of vertical lines.
#' @param y Mapping for y values.
#' If \code{data} is NULL or not input, \code{y} numeric values will be used as is for the plot.
#' In case there is no \code{x} mapping, \code{y} are the y-values of horizontal lines.
#' @param dataMapping \code{XYGDataMapping} class or subclass
#' mapping x, y and aesthetic variables to the variable names of \code{data}.
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
#' @param plotConfiguration \code{PlotConfiguration} object defining the label and background properties of the plot.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a line layer to a \code{ggplot} graphical object.
#' Use optional argument \code{caption} to set legend caption.
#' Since \code{ggplot} manage aesthetic properties across all layers,
#' aesthetic properties defined in \code{plotConfiguration} will apply across all layers.
#' @return A \code{ggplot} graphical object
#' @export
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

  # If data is not input
  # Create new data and its mapping from x and y input
  if (is.null(data)) {
    data <- as.data.frame(cbind(x = x, y = y))

    dataMapping <- dataMapping %||% XYGDataMapping$new(x = ifnotnull(x, "x"), y = ifnotnull(y, "y"), data = data)
  }

  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% XYGDataMapping$new(x = x, y = y, data = data)

  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (is.null(dataMapping$x) && is.null(dataMapping$y)) {
    warning("No mapping found, line layer was not added")
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
  if (is.null(dataMapping$x) && !is.null(dataMapping$y)) {
    plotObject <- plotObject +
      ggplot2::geom_hline(
        data = mapData,
        mapping = aes_string(yintercept = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = aes_string(shape = "legendLabels", fill = "legendLabels")
      )
  }

  # x-intercept
  if (is.null(dataMapping$y) && !is.null(dataMapping$x)) {
    plotObject <- plotObject +
      ggplot2::geom_vline(
        data = mapData,
        mapping = aes_string(xintercept = mapLabels$x, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = aes_string(shape = "legendLabels", fill = "legendLabels")
      )
  }

  if (!is.null(dataMapping$x) && !is.null(dataMapping$y)) {
    plotObject <- plotObject +
      ggplot2::geom_point(
        data = mapData,
        mapping = aes_string(x = mapLabels$x, y = mapLabels$y, shape = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_line(
        data = mapData,
        mapping = aes_string(x = mapLabels$x, y = mapLabels$y, linetype = "legendLabels", color = "legendLabels", size = "legendLabels"),
        show.legend = TRUE
      ) +
      ggplot2::geom_ribbon(
        data = mapData,
        mapping = aes_string(x = mapLabels$x, ymin = mapLabels$y, ymax = mapLabels$y, fill = "legendLabels"),
        show.legend = TRUE
      )
  }

  newLabels <- levels(factor(mapData$legendLabels))
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newLegendProperty <- seq(legendLength + 1, legendLength + length(newLabels)) - 1 %% 6 + 1

  # Sample LegendType properties based tlfTheme if not input
  plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    color = color %||% tlfEnv$currentTheme$aesProperties$color[newLegendProperty],
    shape = shape %||% rep(-2, length(newLabels)),
    size = size %||% rep(1, length(newLabels)),
    linetype = linetype %||% tlfEnv$currentTheme$aesProperties$linetype[newLegendProperty],
    fill = rep(NA, length(newLabels))
  )
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$xAxis$setPlotAxis(plotObject)))
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$yAxis$setPlotAxis(plotObject)))
  return(plotObject)
}

#' @title addRibbon
#' @param data data.frame containing the ribbon endpoints to be plotted.
#' @param metaData list of information on \code{data} such as \code{dimension} and \code{unit} of their variables
#' @param x Mapping for x values.
#' If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.
#' @param ymin Mapping for ymin values.
#' If \code{data} is NULL or not input, \code{ymin} numeric values will be used as is for the plot.
#' In case there is no \code{x} mapping, \code{ymin} are the y-values of horizontal lines.
#' In case there is no \code{ymax} mapping, a value of 0 is assumed for \code{ymax}.
#' @param ymax Mapping for ymax values.
#' If \code{data} is NULL or not input, \code{ymax} numeric values will be used as is for the plot.
#' In case there is no \code{x} mapping, \code{ymax} are the y-values of horizontal lines.
#' In case there is no \code{ymin} mapping, a value of 0 is assumed for \code{ymin}.
#' @param dataMapping \code{XYGDataMapping} class or subclass
#' mapping x, y and aesthetic variables to the variable names of \code{data}.
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
#' @param alpha transparency of the ribbon.
#' Numeric value between 0 and 1. Value of 0, the plot is transparent. Value of 1, the plot is opaque.
#' Default value for \code{alpha} is 0.8.
#' @param plotConfiguration \code{PlotConfiguration} object defining the label and background properties of the plot.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a ribbon layer to a \code{ggplot} graphical object.
#' Use optional argument \code{caption} to set legend caption.
#' Since \code{ggplot} manage aesthetic properties across all layers,
#' aesthetic properties defined in \code{plotConfiguration} will apply across all layers.
#' @return A \code{ggplot} graphical object
#' @export
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

  # If data is not input
  # Create new data and its mapping from x, ymin and ymax input
  if (is.null(data)) {
    data <- as.data.frame(cbind(x = x, ymin = ymin %||% 0, ymax = ymax %||% 0))

    # y-intercept ribbon
    if (is.null(x)) {
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

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (is.null(dataMapping$x) && is.null(dataMapping$ymin) && is.null(dataMapping$ymax)) {
    warning("No mapping found, ribbon layer was not added")
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


  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels

  # y-intercept
  if (max(is.infinite(mapData[, dataMapping$x])) == 1) {
    plotObject <- plotObject +
      ggplot2::geom_ribbon(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$ymax, fill = "legendLabels", color = "legendLabels", size = "legendLabels", linetype = "legendLabels"),
        alpha = alpha,
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = aes_string(shape = "legendLabels")
      )
  } else {
    plotObject <- plotObject +
      ggplot2::geom_ribbon(
        data = mapData,
        mapping = ggplot2::aes_string(x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$ymax, fill = "legendLabels", color = "legendLabels", size = "legendLabels", linetype = "legendLabels"),
        alpha = alpha,
        show.legend = TRUE
      ) +
      ggplot2::geom_blank(
        data = mapData,
        mapping = aes_string(shape = "legendLabels")
      )
  }

  newLabels <- levels(factor(mapData$legendLabels))
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newLegendProperty <- seq(legendLength + 1, legendLength + length(newLabels)) - 1 %% 6 + 1

  # Sample LegendType properties based tlfTheme if not input
  plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    color = color %||% tlfEnv$currentTheme$aesProperties$color[newLegendProperty],
    shape = rep(-2, length(newLabels)),
    size = size %||% rep(1, length(newLabels)),
    linetype = linetype %||% rep("blank", length(newLabels)),
    fill = fill %||% tlfEnv$currentTheme$aesProperties$fill[newLegendProperty]
  )
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$xAxis$setPlotAxis(plotObject)))
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$yAxis$setPlotAxis(plotObject)))
  return(plotObject)
}


#' @title addErrorbar
#' @param data data.frame containing the errorbar endpoints to be plotted
#' @param metaData list of information on \code{data} such as \code{dimension} and \code{unit} of their variables
#' @param x Mapping for x values.
#' If \code{data} is NULL or not input, \code{x} numeric values will be used as is for the plot.
#' @param ymin Mapping for ymin values.
#' If \code{data} is NULL or not input, \code{ymin} numeric values will be used as is for the plot.
#' @param ymax Mapping for ymin values.
#' If \code{data} is NULL or not input, \code{ymax} numeric values will be used as is for the plot.
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
#' @param includeCap logical setting if error bars include caps at their ends.
#' @param dataMapping \code{RangeDataMapping} class or subclass
#' mapping x, ymin, ymax and aesthetic variables to the variable names of \code{data}.
#' @param plotConfiguration \code{PlotConfiguration} object defining the label and background properties of the plot.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a errorbar plot layer to a \code{ggplot} graphical object.
#' Use optional argument \code{caption} to set legend caption.
#' Since \code{ggplot} manage aesthetic properties across all layers,
#' aesthetic properties defined in \code{plotConfiguration} will apply across all layers.
#' If caption is the same as a previous scatter plot layer, the legend will merge their caption and aesthetic properties
#' @return A \code{ggplot} graphical object
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
  validateIsOfType(dataMapping, RangeDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, PlotConfiguration, nullAllowed = TRUE)
  validateIsLogical(includeCap)

  # If data is not input
  # Create new data and its mapping from x, ymin and ymax input
  if (is.null(data)) {
    data <- as.data.frame(cbind(x = x, ymin = ymin %||% 0, ymax = ymax %||% 0))

    dataMapping <- dataMapping %||% RangeDataMapping$new(x = "x", ymin = "ymin", ymax = "ymax", data = data)
  }
  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data, check.names = FALSE)
  }

  dataMapping <- dataMapping %||% RangeDataMapping$new(x = x, ymin = ymin, ymax = ymax, data = data)

  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # If no mapping, return plot
  if (is.null(dataMapping$x) || is.null(dataMapping$ymin) || is.null(dataMapping$ymax)) {
    warning("No mapping found for x, ymin or ymax, errorbar layer was not added")
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

  # Get mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  # If no specific mapping, use default captions
  if (min(levels(factor(mapData$legendLabels)) == "") == 1) {
    mapData$legendLabels <- getlegendLabelsCaption(plotObject)
  }
  mapData$legendLabels <- caption %||% mapData$legendLabels

  if (includeCap) {
    plotObject <- plotObject +
      ggplot2::geom_errorbar(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          ymin = mapLabels$ymin,
          ymax = mapLabels$ymax,
          color = "legendLabels",
          size = "legendLabels",
          linetype = "legendLabels"
        ),
        show.legend = TRUE
      )
  }
  if (!includeCap) {
    plotObject <- plotObject +
      ggplot2::geom_linerange(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          ymin = mapLabels$ymin,
          ymax = mapLabels$ymax,
          color = "legendLabels",
          size = "legendLabels",
          linetype = "legendLabels"
        ),
        show.legend = TRUE
      )
  }
  plotObject <- plotObject +
    ggplot2::geom_blank(
      data = mapData,
      mapping = aes_string(shape = "legendLabels", fill = "legendLabels")
    )

  newLabels <- levels(factor(mapData$legendLabels))
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newLegendProperty <- seq(legendLength + 1, legendLength + length(newLabels)) - 1 %% 6 + 1

  # Sample LegendType properties based tlfTheme if not input
  plotObject <- mergeLegend(plotObject,
    newLabels = newLabels,
    color = color %||% tlfEnv$currentTheme$aesProperties$color[newLegendProperty],
    shape = rep(-2, length(newLabels)),
    size = size %||% rep(1, length(newLabels)),
    linetype = linetype %||% tlfEnv$currentTheme$aesProperties$linetype[newLegendProperty],
    fill = rep(NA, length(newLabels))
  )
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$xAxis$setPlotAxis(plotObject)))
  try(suppressMessages(plotObject <- plotObject$plotConfiguration$yAxis$setPlotAxis(plotObject)))
  return(plotObject)
}

getlegendLabelsCaption <- function(plotObject) {
  legendLabelsCaptionCount <- which(paste0("data ", seq(1, 100)) %in% plotObject$plotConfiguration$legend$caption$label)
  if (length(legendLabelsCaptionCount) == 0) {
    legendLabelsCaptionCount <- 0
  }
  return(paste0("data ", max(legendLabelsCaptionCount) + 1))
}
