#' @title plotTimeProfile
#' @description
#' Producing Time Profile plots
#'
#' @inheritParams addScatter
#' @param dataMapping
#' A `TimeProfileDataMapping` object mapping `x`, `y`, `ymin`, `ymax` and aesthetic groups to their variable names of `data`.
#' @param observedData A data.frame to use for plot.
#' Unlike `data`, meant for simulated data, plotted as lines and ribbons;
#' `observedData` is plotted as scatter points and errorbars.
#' @param observedDataMapping
#' An `ObservedDataMapping` object mapping `x`, `y`, `ymin`, `ymax` and aesthetic groups to their variable names of `observedData`.
#' @param plotConfiguration
#' An optional `TimeProfilePlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce a Time profile plot with observed and simulated data
#' obsData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#' simTime <- seq(1, 10, 0.1)
#' simData <- data.frame(
#'   x = simTime,
#'   y = 10 * exp(-simTime),
#'   ymin = 8 * exp(-simTime),
#'   ymax = 12 * exp(-simTime)
#' )
#'
#' plotTimeProfile(
#'   data = simData,
#'   observedData = obsData,
#'   dataMapping = TimeProfileDataMapping$new(x = "x", y = "y", ymin = "ymin", ymax = "ymax"),
#'   observedDataMapping = ObservedDataMapping$new(x = "x", y = "y")
#' )
plotTimeProfile <- function(data = NULL,
                            metaData = NULL,
                            dataMapping = NULL,
                            observedData = NULL,
                            observedDataMapping = NULL,
                            plotConfiguration = NULL,
                            plotObject = NULL) {
  validateIsOfType(data, "data.frame", nullAllowed = TRUE)
  validateIsOfType(observedData, "data.frame", nullAllowed = TRUE)
  if (all(isEmpty(data), isEmpty(observedData))) {
    warning("'data' and 'observedData' are of length 0. Time profile layer was not added.")
    return(plotObject)
  }

  if (!isEmpty(data)) {
    dataMapping <- dataMapping %||% TimeProfileDataMapping$new(data = data)
  }
  if (!isEmpty(observedData)) {
    observedDataMapping <- observedDataMapping %||% ObservedDataMapping$new(data = data)
  }

  plotConfiguration <- plotConfiguration %||% TimeProfilePlotConfiguration$new(data = data, metaData = metaData, dataMapping = dataMapping)

  validateIsOfType(dataMapping, TimeProfileDataMapping, nullAllowed = TRUE)
  validateIsOfType(observedDataMapping, ObservedDataMapping, nullAllowed = TRUE)
  validateIsOfType(plotConfiguration, TimeProfilePlotConfiguration)

  # Initialize plot based on plotConfiguration
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  if (!isEmpty(data)) {
    mapData <- dataMapping$checkMapData(data)
    mapLabels <- getAesStringMapping(dataMapping)
    # Initialize variables used in legend caption
    fillValues <- NULL
    linetypeValues <- NULL
    if (!any(isEmpty(dataMapping$ymin), isEmpty(dataMapping$ymax))) {
      plotObject <- plotObject +
        ggplot2::geom_ribbon(
          data = mapData,
          mapping = ggplot2::aes_string(
            x = mapLabels$x,
            ymin = mapLabels$ymin,
            ymax = mapLabels$ymax,
            fill = mapLabels$fill
          ),
          alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$alpha, position = 0, aesthetic = "alpha"),
          show.legend = TRUE
        )
    }
    if (!isEmpty(dataMapping$y)) {
      plotObject <- plotObject +
        ggplot2::geom_path(
          data = mapData,
          mapping = ggplot2::aes_string(
            x = mapLabels$x,
            y = mapLabels$y,
            color = mapLabels$color,
            linetype = mapLabels$linetype
          ),
          size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = 0, aesthetic = "size"),
          alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = 0, aesthetic = "alpha"),
          show.legend = TRUE
        )
    }
    eval(parseUpdateAestheticProperty(AestheticProperties$fill, "ribbons"))
    eval(parseUpdateAestheticProperty(AestheticProperties$linetype, "lines"))
    # fillLength defined in parseUpdateAestheticProperty
    fillValues <- getAestheticValues(
      n = fillLength,
      selectionKey = plotConfiguration$ribbons$fill,
      aesthetic = "fill"
    )
    linetypeValues <- getAestheticValues(
      n = linetypeLength,
      selectionKey = plotConfiguration$lines$linetype,
      aesthetic = "linetype"
    )
  }

  # If no observed data, also update colors and return plotObect
  if (isEmpty(observedData)) {
    eval(parseUpdateAestheticProperty(AestheticProperties$color, "lines"))
    eval(parseUpdateAxes())
    # Update and match legend caption to properties
    # colorLength defined in parseUpdateAestheticProperty
    colorValues <- getAestheticValues(
      n = colorLength,
      selectionKey = plotConfiguration$lines$color,
      aesthetic = "color"
    )
    plotObject$plotConfiguration$legend$caption <- data.frame(
      name = levels(mapData[, colorVariable]),
      label = levels(mapData[, colorVariable]),
      color = colorValues,
      fill = fillValues %||% NA,
      linetype = linetypeValues %||% "blank",
      shape = " ",
      stringsAsFactors = FALSE
    )
    eval(parseUpdateAxes())
    return(plotObject)
  }

  mapObservedData <- observedDataMapping$checkMapData(observedData)
  observedMapLabels <- getAesStringMapping(observedDataMapping)

  if (!any(isEmpty(observedDataMapping$ymin), isEmpty(observedDataMapping$ymax))) {
    # Split errorbars for negative data and log scaling
    plotObject <- plotObject +
      ggplot2::geom_linerange(
        data = mapObservedData,
        mapping = ggplot2::aes_string(
          x = observedMapLabels$x,
          ymin = observedMapLabels$ymin,
          ymax = observedMapLabels$y,
          color = observedMapLabels$color
        ),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, position = 0, aesthetic = "linetype"),
        alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$alpha, position = 0, aesthetic = "alpha"),
        show.legend = FALSE
      ) +
      ggplot2::geom_linerange(
        data = mapObservedData,
        mapping = ggplot2::aes_string(
          x = observedMapLabels$x,
          ymin = observedMapLabels$y,
          ymax = observedMapLabels$ymax,
          color = observedMapLabels$color
        ),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),
        linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, position = 0, aesthetic = "linetype"),
        alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$alpha, position = 0, aesthetic = "alpha"),
        show.legend = FALSE
      )
  }
  plotObject <- plotObject +
    ggplot2::geom_point(
      data = mapObservedData,
      mapping = ggplot2::aes_string(
        x = observedMapLabels$x,
        y = observedMapLabels$y,
        color = observedMapLabels$color,
        shape = observedMapLabels$shape
      ),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$points$alpha, position = 0, aesthetic = "alpha"),
      show.legend = TRUE
    )

  # Update shapes
  # Code chunk below is equivalent to commented expression with a change of variable names
  # parseUpdateAestheticProperty(AestheticProperties$shape, "points")
  shapeVariable <- gsub("`", "", observedMapLabels$shape)
  shapeLength <- length(levels(mapObservedData[, shapeVariable]))
  shapeValues <- getAestheticValues(
    n = shapeLength,
    selectionKey = plotConfiguration$points$shape,
    aesthetic = "shape"
  )
  suppressMessages(plotObject <- plotObject + ggplot2::scale_shape_manual(values = shapeValues))
  if (isIncluded(shapeVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(shape = "none")
  }

  # Update colors,
  # Since colors can be available in both simulated and observed, the commented expressions can't apply
  # parseUpdateAestheticProperty(AestheticProperties$color, "lines")
  # parseUpdateAestheticProperty(AestheticProperties$color, "points")

  # No simulated data -> update only observedData
  if (isEmpty(data)) {
    colorVariable <- gsub("`", "", observedMapLabels$color)
    colorLength <- length(levels(mapObservedData[, colorVariable]))
    colorValues <- getAestheticValues(
      n = colorLength,
      selectionKey = plotConfiguration$points$color,
      aesthetic = "color"
    )

    suppressMessages(
      plotObject <- plotObject + ggplot2::scale_color_manual(values = colorValues)
    )
    if (isIncluded(colorVariable, "legendLabels")) {
      plotObject <- plotObject + ggplot2::guides(color = "none")
    }
    # Update and match legend caption to properties
    plotObject$plotConfiguration$legend$caption <- data.frame(
      name = levels(mapObservedData[, colorVariable]),
      label = levels(mapObservedData[, colorVariable]),
      color = colorValues,
      fill = NA,
      linetype = "blank",
      shape = shapeValues %||% NA,
      stringsAsFactors = FALSE
    )

    eval(parseUpdateAxes())
    return(plotObject)
  }

  # Simulated and Observed data -> need to merge the legends
  colorVariable <- gsub("`", "", mapLabels$color)
  colorLength <- length(levels(mapData[, colorVariable]))
  colorObservedVariable <- gsub("`", "", observedMapLabels$color)
  # The final color vector needs a length of totalLength to prevent scale_color_manual to crash
  colorBreaks <- c(
    levels(mapData[, colorVariable]),
    setdiff(levels(mapObservedData[, colorObservedVariable]), levels(mapData[, colorVariable]))
  )
  totalLength <- length(colorBreaks)

  # colorValues is colors for simulated and then colors for observed
  colorValues <- c(
    getAestheticValues(
      n = colorLength,
      selectionKey = plotConfiguration$lines$color,
      aesthetic = "color"
    ),
    getAestheticValues(
      n = totalLength - colorLength,
      selectionKey = plotConfiguration$points$color,
      aesthetic = "color"
    )
  )

  # Export the legend captions so the user can update legend keys order
  plotObject$plotConfiguration$legend$caption <- data.frame(
    name = colorBreaks,
    label = colorBreaks,
    color = colorValues,
    fill = c(fillValues, rep(NA, totalLength - fillLength)),
    linetype = c(linetypeValues, rep("blank", totalLength - linetypeLength)),
    shape = c(rep(" ", totalLength - shapeLength), shapeValues),
    stringsAsFactors = FALSE
  )

  plotObject <- updateTimeProfileLegend(
    plotObject = plotObject,
    caption = plotObject$plotConfiguration$legend$caption
  )

  if (isIncluded(colorVariable, "legendLabels") & isIncluded(colorObservedVariable, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(color = "none")
  }

  eval(parseUpdateAxes())
  return(plotObject)
}


#' @title updateTimeProfileLegend
#' @description Update time profile legend caption
#' @param plotObject A ggplot object
#' @param caption A data.frame as obtained from `getLegendCaption` to use for updating a plot legend.
#' @return A `ggplot` object
#' @export
updateTimeProfileLegend <- function(plotObject, caption) {
  # Update defined aesthetic properies
  captionLinetype <- caption[caption$linetype != "blank", ]
  captionShape <- caption[caption$shape != " ", ]
  captionFill <- caption[!is.na(caption$fill), ]

  if (!isEmpty(captionLinetype)) {
    suppressMessages(
      plotObject <- plotObject +
        ggplot2::scale_linetype_manual(breaks = captionLinetype$name, labels = captionLinetype$label, values = captionLinetype$linetype)
    )
  }
  if (!isEmpty(captionShape)) {
    suppressMessages(
      plotObject <- plotObject +
        ggplot2::scale_shape_manual(breaks = captionShape$name, labels = captionShape$label, values = captionShape$shape)
    )
  }
  if (!isEmpty(captionFill)) {
    suppressMessages(
      plotObject <- plotObject +
        ggplot2::scale_fill_manual(breaks = captionFill$name, labels = captionFill$label, values = captionFill$fill)
    )
  }

  suppressMessages(
    plotObject <- plotObject +
      ggplot2::scale_color_manual(breaks = caption$name, labels = caption$label, values = caption$color) +
      ggplot2::guides(
        fill = "none", shape = "none", linetype = "none",
        color = ggplot2::guide_legend(
          override.aes = list(fill = caption$fill, linetype = caption$linetype, shape = caption$shape)
        )
      )
  )
  plotObject$plotConfiguration$legend$caption <- caption
  return(plotObject)
}
