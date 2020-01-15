# Atom plots

#' @title initializePlot
#' @param plotConfiguration
#' \code{PlotConfiguration} class or subclass defining labels, grid, background and watermark
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
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new()

  plotObject <- ggplot2::ggplot()
  plotObject <- plotConfiguration$setPlotBackground(plotObject)
  plotObject <- plotConfiguration$setPlotLabels(plotObject)

  return(plotObject)
}


#' @title addScatterPlot
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' \code{XYGDataMapping} class or subclass mapping x and y variables to \code{data} variable names
#' This parameter is optional: the \code{tlf} library provides a smart mapping if only \code{data} is provided
#' @param plotConfiguration
#' \code{PlotConfiguration} class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @param plotObject \code{ggplot} graphical object to which the scatter plot layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a scatter plot layer to a \code{ggplot} graphical object
#' @return A \code{ggplot} graphical object
#' @export
#' @examples
#' data <- data.frame(x = sample(seq(0, 1, 0.001), size = 100), 
#' y = sample(seq(0, 10, 0.001), size = 100))
#' addScatterPlot(data)
addScatterPlot <- function(data,
                           metaData = NULL,
                           dataMapping = NULL,
                           plotConfiguration = NULL,
                           plotObject = NULL) {
  dataMapping <- dataMapping %||% XYGDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% PlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  validateIsOfType(dataMapping, XYDataMapping)
  validateIsOfType(plotConfiguration, PlotConfiguration)

  # Check if mapping is included in the data
  # Add the group mapping and aesthtics variables in the data.frame
  mapData <- dataMapping$checkMapData(data, metaData)

  # Convert the mapping into characters usable by aes_string
  mapLabels <- getAesStringMapping(dataMapping)

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  plotObject <- plotObject + geom_point(
    data = mapData,
    mapping = aes_string(
      x = mapLabels$x,
      y = mapLabels$y,
      color = mapLabels$color,
      shape = mapLabels$shape,
      size = mapLabels$size
    ),
    show.legend = TRUE
  )

  return(plotObject)
}

#' @title addLine
#' @param data data.frame containing the line endpoints to be plotted
#' @param x numeric value for vertical line
#' @param y numeric value for horizontal line
#' @param dataMapping \code{XYDataMapping} class or subclass
#' mapping x and y variables to the variable names of \code{data}.
#' @param color character string defining the color of the line.
#' This parameter is optional: default value is "black".
#' @param linetype character string defining the type of the line (plain, dash, dotted ...).
#' This parameter is optional: default value is "solid".
#' @param size numerical defining the width of the line.
#' This parameter is optional: default value is 1.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a line layer to a \code{ggplot} graphical object
#' @return A \code{ggplot} graphical object
#' @export
#' @examples
#' # Add vertical line at x = 2
#' addLine(x = 2)
#'
#' # Add vertical line at x = 2 to a previous plot
#' p <- initializePlot() + 
#' ggplot2::geom_point(data = data.frame(x = c(1, 2, 3), y = c(5, 6, 4)),
#'  mapping = ggplot2::aes_string(x = "x", y = "y"))
#' p <- addLine(x = 2, plotObject = p)
#'
#' # Add horizontal line at y = 5 to a previous plot
#' p <- addLine(y = 5, plotObject = p)
#'
#' # Add a custom line
#' customLineData <- data.frame(x = c(0, 1, 2), y = c(0, 1, 1))
#' p <- addLine(data = customLineData, 
#' dataMapping = LineDataMapping$new(x = "x", y = "y"))
#'
#' # Or for simple cases a smart mapping will get directly x and y from data
#' p <- addLine(data = customLineData)
#'
#' # Add a line with custom aesthetic properties
#' p <- addLine(data = customLineData, color = "blue", linetype = "longdash", size = 0.5)
addLine <- function(data = NULL,
                    x = NULL,
                    y = NULL,
                    dataMapping = NULL,
                    color = "black",
                    linetype = "solid",
                    size = 1,
                    plotObject = NULL) {

  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data)
  }

  dataMapping <- dataMapping %||% LineDataMapping$new(
    x = x,
    y = y,
    data = data
  )
  validateIsOfType(dataMapping, XYDataMapping)

  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot()

  # If no mapping, return plot
  if (is.null(dataMapping$x) && is.null(dataMapping$y)) {
    return(plotObject)
  }

  # y-intercept
  if (is.null(dataMapping$x) && !is.null(dataMapping$y)) {
    plotObject <- plotObject +
      ggplot2::geom_hline(
        yintercept = dataMapping$y,
        linetype = linetype,
        color = color,
        size = size
      )
    return(plotObject)
  }

  # x-intercept
  if (is.null(dataMapping$y) && !is.null(dataMapping$x)) {
    plotObject <- plotObject +
      ggplot2::geom_vline(
        xintercept = dataMapping$x,
        linetype = linetype,
        color = color,
        size = size
      )
    return(plotObject)
  }

  # Else regular line from data endpoints

  mapData <- dataMapping$checkMapData(data)
  # Convert the mapping into characters usable by aes_string
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject +
    ggplot2::geom_line(
      data = mapData,
      mapping = aes_string(
        x = mapLabels$x,
        y = mapLabels$y
      ),
      linetype = linetype,
      color = color,
      size = size
    )

  return(plotObject)
}

#' @title addRibbon
#' @param data data.frame containing the ribbon endpoints to be plotted.
#' @param x numeric value for vertical line.
#' @param ymin numeric value for lower horizontal limit. 
#' A value of 0 is assumed if only \code{ymax} value is input.
#' @param ymax numeric value for upper horizontal limit.
#' A value of 0 is assumed if only \code{ymin} value is input.
#' @param dataMapping \code{RangeDataMapping} class or subclass
#' mapping x, ymin and ymax variables to the variable names of \code{data}.
#' @param fill character string defining the color of the ribbon.
#' This parameter is optional: default value is "grey".
#' @param alpha numerical value between 0 and 1 defining the transparency of the ribbon.
#' This parameter is optional: default value is 0.9.
#' @param plotObject \code{ggplot} graphical object to which the line layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add a ribbon layer to a \code{ggplot} graphical object.
#' @return A \code{ggplot} graphical object
#' @export
#' @examples
#' # Add a horizontal ribbon to a previous plot
#' p <- ggplot2::ggplot()
#' pmin <- addRibbon(ymin = -5, plotObject = p)
#' pmax <- addRibbon(ymax = 5, plotObject = p)
#' 
#' p <- addRibbon(ymin = -5, ymax = 5, plotObject = p)
#'
#' # Add a custom ribbon
#' time <- seq(0,30,0.01)
#' customRibbonData <- data.frame(x=time, ymin = cos(time), ymax = cos(time)+1)
#' 
#' p <- addRibbon(data=customRibbonData,
#' dataMapping = RangeDataMapping$new(x="x",ymin="ymin",ymax="ymax"))
#'
#' # Or for simple cases a smart mapping will get directly x and y from data
#' p <- addRibbon(data = customRibbonData)
#'
#' # Add a ribbon with custom aesthetic properties
#' p <- addRibbon(data = customRibbonData, fill = "#34eb89", alpha = 0.5)
#' 
addRibbon <- function(data = NULL,
                    x = NULL,
                    ymin = NULL,
                    ymax = NULL,
                    dataMapping = NULL,
                    fill = "grey",
                    alpha = 0.9,
                    plotObject = NULL) {
  
  # Enforce data to be a data.frame for dataMapping
  if (!isOfType(data, "data.frame")) {
    data <- data.frame(data)
  }
  
  dataMapping <- dataMapping %||% RangeDataMapping$new(
    x = x,
    ymin = ymin,
    ymax = ymax,
    data = data
  )
  validateIsOfType(dataMapping, RangeDataMapping)
  
  # If no plot, initialize empty plot
  plotObject <- plotObject %||% initializePlot()
  
  # If no mapping, return plot
  if (is.null(dataMapping$x) && is.null(dataMapping$ymin) && is.null(dataMapping$ymax)) {
    return(plotObject)
  }
  
  # Define a data.frame for y ribbon intercept
  interceptData <- data.frame(x = c(-Inf, Inf),
                              ymin = rep(dataMapping$ymin %||% 0, 2),
                              ymax = rep(dataMapping$ymax %||% 0, 2))
  
  if (is.null(dataMapping$x)){
    plotObject <- plotObject +
      ggplot2::geom_ribbon(
        data = interceptData,
        mapping = aes_string(x= "x", ymin = "ymin", ymax = "ymax"),
        fill = fill,
        alpha = alpha
      )
    return(plotObject)
  }
  
  # Else regular line from data endpoints
  mapData <- dataMapping$checkMapData(data)
  # Convert the mapping into characters usable by aes_string
  mapLabels <- getAesStringMapping(dataMapping)
  
  plotObject <- plotObject + ggplot2::geom_ribbon(
    data = mapData,
    mapping = aes_string(x = mapLabels$x,
                         ymin = mapLabels$ymin, 
                         ymax = mapLabels$ymax),
    fill = fill,
    alpha =alpha)
  
  return(plotObject)
}
