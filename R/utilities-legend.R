#' @title setLegend
#' @param plotObject Graphical object created from ggplot
#' @param position legend position.
#' Use enum `LegendPositions` to access the list of legend positions.
#' @param title title of legend
#' @param caption data.frame containing the caption properties of the legend
#' @return A \code{ggplot} graphical object
#' @description
#' Set legend position, title and/or caption
#' @export
setLegend <- function(plotObject,
                      position,
                      title = NULL,
                      caption = NULL) {
  validateIsIncluded(position, LegendPositions)
  validateIsOfType(plotObject, "ggplot")

  plotObject <- setLegendPosition(plotObject, position)
  plotObject <- setLegendTitle(plotObject, title)

  if (!is.null(caption)) {
    plotObject <- setLegendCaption(plotObject, caption)
  }
  return(plotObject)
}

#' @title setLegendCaption
#' @param plotObject \code{ggplot} graphical object
#' @param caption data.frame containing the caption properties of the legend
#' @return A \code{ggplot} graphical object
#' @description
#' Set the legend caption
#' @export
#' @import ggplot2
setLegendCaption <- function(plotObject,
                             caption) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(caption, "data.frame")
  validateIsIncluded(names(caption), CaptionProperties)

  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  legend <- newPlotObject$plotConfiguration$legend
  legend$caption <- caption

  # Re-order based on order variable
  orderedName <- legend$caption$name[legend$caption$order]
  orderedLabel <- legend$caption$label[legend$caption$order]
  orderedVisibility <- legend$caption$visibility[legend$caption$order]

  # The next lines need to be looped on every element of LegendTypes
  # Otherwise, there can be a legend per LegendTypes
  # breaks and values don't need to match,
  # however values is remapped on ggplot alphabetical order to get the right values
  for (aestype in LegendTypes) {
    # scale_discrete_manual sends warning every time it overwrite something
    # so this line need to be silent
    suppressMessages(
      newPlotObject <- newPlotObject +
        ggplot2::scale_discrete_manual(
          aesthetics = aestype,
          name = legend$title,
          breaks = orderedName[orderedVisibility],
          labels = orderedLabel[orderedVisibility],
          values = legend$caption[order(legend$caption$name), aestype]
        )
    )
  }
  return(newPlotObject)
}

#' @title setLegendTitle
#' @param plotObject \code{ggplot} graphical object
#' @param title title of legend
#' @return A \code{ggplot} graphical object
#' @description
#' Set the legend title
#' @export
#' @import ggplot2
setLegendTitle <- function(plotObject,
                           title) {
  validateIsOfType(plotObject, "ggplot")

  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  legend <- newPlotObject$plotConfiguration$legend
  legend$title <- title

  # Re-order based on order variable
  orderedName <- legend$caption$name[legend$caption$order]
  orderedLabel <- legend$caption$label[legend$caption$order]
  orderedVisibility <- legend$caption$visibility[legend$caption$order]

  # The next lines need to be looped on every element of LegendTypes
  # Otherwise, there can be a title per LegendTypes
  for (aestype in LegendTypes) {
    # scale_discrete_manual sends warning every time it overwrite something
    # besides the function need value input to be filled otherwise crash
    # so this line need to be silent
    suppressMessages(
      newPlotObject <- newPlotObject +
        ggplot2::scale_discrete_manual(
          aesthetics = aestype,
          name = legend$title,
          breaks = orderedName[orderedVisibility],
          labels = orderedLabel[orderedVisibility],
          values = legend$caption[order(legend$caption$name), aestype]
        )
    )
  }
  return(newPlotObject)
}

#' @title setLegendTitle
#' @param plotObject \code{ggplot} graphical object
#' @param position legend position.
#' Use enum `LegendPositions` to access the list of legend positions.
#' @return A \code{ggplot} graphical object
#' @description
#' Set the legend position
#' @export
#' @import ggplot2
setLegendPosition <- function(plotObject,
                              position) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(position, LegendPositions)

  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  legend <- newPlotObject$plotConfiguration$legend
  legend$position <- position

  legendPosition <- getLegendPosition(legend$position)

  newPlotObject <- newPlotObject + theme(
    legend.position = c(legendPosition$xPosition, legendPosition$yPosition),
    legend.justification = c(legendPosition$xJustification, legendPosition$yJustification)
  )

  return(newPlotObject)
}

#' LegendPositions
#'
#' @include enum.R
#' @export
#' @description
#' List of all available legend positions
#'
LegendPositions <- enum(c(
  "none",
  "insideTop",
  "insideTopLeft",
  "insideLeft",
  "insideBottomLeft",
  "insideBottom",
  "insideBottomRight",
  "insideRight",
  "insideTopRight",
  "outsideTop",
  "outsideTopLeft",
  "outsideLeft",
  "outsideBottomLeft",
  "outsideBottom",
  "outsideBottomRight",
  "outsideRight",
  "outsideTopRight"
))

#' LegendTypes
#'
#' @include enum.R
#' @export
#' @description
#' List of all available legend types
LegendTypes <- enum(c(
  "color",
  "shape",
  "linetype",
  "fill",
  "size"
))

CaptionProperties <- enum(c(
  "name",
  "label",
  "visibility",
  "order",
  "color",
  "shape",
  "linetype",
  "size",
  "fill"
))

getLegendPosition <- function(position) {
  validateIsIncluded(position, LegendPositions)

  listOfLegendPositions <- list(
    none = list(xPosition = "none", xJustification = NULL, yPosition = NULL, yJustification = NULL),
    insideTop = list(xPosition = 0.5, xJustification = 0.5, yPosition = 0.975, yJustification = 1),
    insideTopLeft = list(xPosition = 0.025, xJustification = 0, yPosition = 0.975, yJustification = 1),
    insideLeft = list(xPosition = 0.025, xJustification = 0, yPosition = 0.5, yJustification = 0.5),
    insideBottomLeft = list(xPosition = 0.025, xJustification = 0, yPosition = 0.025, yJustification = 0),
    insideBottom = list(xPosition = 0.5, xJustification = 0.5, yPosition = 0.025, yJustification = 0),
    insideBottomRight = list(xPosition = 0.975, xJustification = 1, yPosition = 0.025, yJustification = 0),
    insideRight = list(xPosition = 0.975, xJustification = 1, yPosition = 0.5, yJustification = 0.5),
    insideTopRight = list(xPosition = 0.975, xJustification = 1, yPosition = 0.975, yJustification = 1),
    outsideTop = list(xPosition = NULL, xJustification = 0.5, yPosition = "top", yJustification = NULL),
    outsideTopLeft = list(xPosition = NULL, xJustification = 0, yPosition = "top", yJustification = 0.5),
    outsideLeft = list(xPosition = "left", xJustification = NULL, yPosition = NULL, yJustification = NULL),
    outsideBottomLeft = list(xPosition = NULL, xJustification = 0, yPosition = "bottom", yJustification = 0.5),
    outsideBottom = list(xPosition = NULL, xJustification = 0.5, yPosition = "bottom", yJustification = NULL),
    outsideBottomRight = list(xPosition = NULL, xJustification = 1, yPosition = "bottom", yJustification = 0.5),
    outsideRight = list(xPosition = "right", xJustification = NULL, yPosition = NULL, yJustification = NULL),
    outsideTopRight = list(xPosition = NULL, xJustification = 1, yPosition = "top", yJustification = 0.5)
  )

  legendPosition <- listOfLegendPositions[[position]]
  return(legendPosition)
}

mergeLegend <- function(plotObject,
                        newLabels,
                        color,
                        shape,
                        size,
                        linetype,
                        fill) {
  validateIsOfType(plotObject, "ggplot")

  oldCaption <- plotObject$plotConfiguration$legend$caption
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newCaption <- data.frame(
    name = newLabels,
    label = newLabels,
    visibility = rep(TRUE, length(newLabels)),
    order = seq(legendLength + 1, legendLength + length(newLabels)),
    color = color,
    shape = shape,
    size = size,
    linetype = linetype,
    fill = fill,
    stringsAsFactors = FALSE
  )

  mergeCaption <- rbind.data.frame(
    oldCaption,
    newCaption
  )
  plotObject <- setLegendCaption(plotObject, mergeCaption)

  return(plotObject)
}
