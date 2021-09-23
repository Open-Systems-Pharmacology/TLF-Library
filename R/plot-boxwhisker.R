#' @title plotBoxWhisker
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param outliers logical defining if outliers should be included in boxplot
#' @param dataMapping `BoxWhiskerDataMapping` object
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @param plotConfiguration `BoxWhiskerConfiguration` object
#' Plot Configuration defining title, subtitle, xlabel, ylabel watermark, and legend
#' @param plotObject
#' ggplot object, if null creates new plot, if not add time profile layers to ggplot
#' @description
#' plotBoxWhisker(data, metaData, dataMapping, plotConfiguration)
#' @return a ggplot graphical object
#' @export
plotBoxWhisker <- function(data,
                           metaData = NULL,
                           outliers = NULL,
                           dataMapping = NULL,
                           plotConfiguration = NULL,
                           plotObject = NULL) {
  dataMapping <- dataMapping %||% BoxWhiskerDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% BoxWhiskerPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, "BoxWhiskerDataMapping")
  validateIsOfType(plotConfiguration, "BoxWhiskerPlotConfiguration")
  validateIsOfType(data, "data.frame")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  # Overwrites plotConfiguration$outliers if outliers is not null
  validateIsLogical(outliers, nullAllowed = TRUE)
  plotConfiguration$outliers <- outliers

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("box whisker plot"))
    return(plotObject)
  }

  # Add Plot Configuration layers and box whisker plots
  plotObject <- addBoxWhisker(data, metaData, dataMapping, plotConfiguration, plotObject)
  if (plotConfiguration$outliers) {
    plotObject <- addOutliers(data, metaData, dataMapping, plotConfiguration, plotObject)
  }
  plotObject <- setLegendPosition(plotObject)
  plotObject <- setLegendFont(plotObject)
  try(suppressMessages(plotObject <- setXAxis(plotObject)))
  try(suppressMessages(plotObject <- setYAxis(plotObject)))
  return(plotObject)
}

#' @title addBoxWhisker
#' @description Add layer of boxes and whiskers  to a `ggplot` object
#' @param data data.frame
#' @param metaData list of information on `data`
#' @param dataMapping `BoxWhiskerDataMapping` object
#' @param plotConfiguration `BoxWhiskerPlotConfiguration` object
#' @param plotObject a `ggplot` object
#' @return A `ggplot` object
addBoxWhisker <- function(data, metaData, dataMapping, plotConfiguration, plotObject) {

  # Get the box plot quantiles from dataMapping
  mapData <- dataMapping$getBoxWhiskerLimits(data)
  # Convert the mapping into characters usable by aes_string
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject +
    ggplot2::geom_boxplot(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        ymin = "ymin",
        lower = "lower",
        middle = "middle",
        upper = "upper",
        ymax = "ymax",
        fill = mapLabels$fill,
        color = mapLabels$color
      ),
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$alpha, position = 0, aesthetic = "alpha"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$size, position = 0, aesthetic = "size"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$linetype, position = 0, aesthetic = "linetype"),
      show.legend = TRUE,
      stat = "identity"
    )

  # Define linetype, color, f# Define shapes and colors based on plotConfiguration$points properties
  fillVariable <- gsub("`", "", mapLabels$fill)
  colorVariable <- gsub("`", "", mapLabels$color)
  fillLength <- length(unique(mapData[, fillVariable]))
  colorLength <- length(unique(mapData[, colorVariable]))

  plotObject <- plotObject +
    ggplot2::scale_fill_manual(values = getAestheticValues(n = fillLength, selectionKey = plotConfiguration$ribbons$fill, aesthetic = "fill")) +
    ggplot2::scale_color_manual(values = getAestheticValues(n = colorLength, selectionKey = plotConfiguration$ribbons$color, aesthetic = "color"))

  # If variable is legendLabel, remove it from legend
  if (isIncluded(fillVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(fill = "none")
  }
  if (isIncluded(colorVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(color = "none")
  }
  return(plotObject)
}

#' @title addOutliers
#' @description Add a outlier points layer to a `ggplot` object
#' @param data data.frame
#' @param metaData list of information on `data`
#' @param dataMapping `BoxWhiskerDataMapping` object
#' @param plotConfiguration `BoxWhiskerPlotConfiguration` object
#' @param plotObject a `ggplot` object
#' @return A `ggplot` object
addOutliers <- function(data, metaData, dataMapping, plotConfiguration, plotObject) {
  mapData <- dataMapping$getOutliers(data)
  # Convert the mapping into characters usable by aes_string
  mapLabels <- getAesStringMapping(dataMapping)

  # addScatter cannot be used in this case,
  # because position dodge is needed to align boxes and outlier points
  # no matter the number of groups, the value of 0.9 will be always fix
  # otherwise, points won't be centered anymore
  # besides, mapData includes NA instead of non-outlying data,
  # na.rm removes these points without sending warning
  plotObject <- plotObject +
    ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = "maxOutliers",
        group = mapLabels$fill,
        color = mapLabels$color
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      shape = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$shape, position = 0, aesthetic = "shape"),
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$color, position = 0, aesthetic = "color"),
      show.legend = TRUE,
      na.rm = TRUE,
      position = position_dodge(width = 0.9)
    ) +
    ggplot2::geom_point(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = "minOutliers",
        group = mapLabels$fill,
        color = mapLabels$color
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      shape = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$shape, position = 0, aesthetic = "shape"),
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$color, position = 0, aesthetic = "color"),
      show.legend = TRUE,
      na.rm = TRUE,
      position = position_dodge(width = 0.9)
    )
  return(plotObject)
}
