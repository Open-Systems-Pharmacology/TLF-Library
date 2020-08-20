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
#' @import utils
setLegendCaption <- function(plotObject, caption) {
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
  for (aestype in LegendTypes) {
    # breaks and values are remapped by alphabetical order on ggplot2 versions <3.3.0
    # For these versions, the order is reverse engineered
    aesValues <- c(
      legend$caption[legend$caption$order[orderedVisibility], aestype],
      legend$caption[legend$caption$order[!orderedVisibility], aestype]
    )
    if (utils::packageVersion("ggplot2") < "3.3.0") {
      aesValues <- legend$caption[order(legend$caption$name), aestype]
    }
    # scale_discrete_manual sends warning every time it overwrite something
    # so this line need to be silent
    suppressMessages(
      newPlotObject <- newPlotObject +
        ggplot2::scale_discrete_manual(
          aesthetics = aestype,
          name = legend$title,
          breaks = orderedName[orderedVisibility],
          labels = orderedLabel[orderedVisibility],
          values = aesValues
        )
    )
  }
  return(newPlotObject)
}

#' @title getLegendCaption
#' @param plotObject \code{ggplot} graphical object
#' @return A data.frame corresponding to legend caption
#' @description
#' Get the legend caption
#' @export
#' @import ggplot2
getLegendCaption <- function(plotObject) {
  validateIsOfType(plotObject, "ggplot")

  # Clone the properties object to prevent spreading of modifications
  # to the plotObject itself
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  caption <- newPlotObject$plotConfiguration$legend$caption

  return(caption)
}

#' @title setCaptionLabel
#' @param plotObject \code{ggplot} graphical object
#' @param label new labels of the legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the legend caption labels
#' @export
#' @import ggplot2
setCaptionLabels <- function(plotObject, label, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(label, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  labelExpression <- getCoreSetCaptionProperty("label")
  eval(labelExpression)

  return(newPlotObject)
}

#' @title setCaptionVisibility
#' @param plotObject \code{ggplot} graphical object
#' @param visibility logical visibility of the legend caption labels
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the visibility of the legend caption labels
#' @export
#' @import ggplot2
setCaptionVisibility <- function(plotObject, visibility, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsLogical(visibility)

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  visibilityExpression <- getCoreSetCaptionProperty("visibility")
  eval(visibilityExpression)

  return(newPlotObject)
}

#' @title setCaptionOrder
#' @param plotObject \code{ggplot} graphical object
#' @param order numeric order of the legend ccaption labels
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the order of the legend caption labels
#' @export
#' @import ggplot2
setCaptionOrder <- function(plotObject, order, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(order, "numeric")

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  orderExpression <- getCoreSetCaptionProperty("order")
  eval(orderExpression)

  return(newPlotObject)
}

#' @title setCaptionColor
#' @param plotObject \code{ggplot} graphical object
#' @param color colors of the data in plot and legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the colors of the data in plot and legend caption
#' @export
#' @import ggplot2
setCaptionColor <- function(plotObject, color, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(color, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  colorExpression <- getCoreSetCaptionProperty("color")
  eval(colorExpression)

  return(newPlotObject)
}

#' @title setCaptionShape
#' @param plotObject \code{ggplot} graphical object
#' @param shape shapes of the data in plot and legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the shapes of the data in plot and legend caption
#' @export
#' @import ggplot2
setCaptionShape <- function(plotObject, shape, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(shape, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  shapeExpression <- getCoreSetCaptionProperty("shape")
  eval(shapeExpression)

  return(newPlotObject)
}

#' @title setCaptionSize
#' @param plotObject \code{ggplot} graphical object
#' @param size sizes of the data in plot and legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the sizes of the data in plot and legend caption
#' @export
#' @import ggplot2
setCaptionSize <- function(plotObject, size, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(size, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  sizeExpression <- getCoreSetCaptionProperty("size")
  eval(sizeExpression)

  return(newPlotObject)
}

#' @title setCaptionLinetype
#' @param plotObject \code{ggplot} graphical object
#' @param linetype linetypes of the data in plot and legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the linetypes of the data in plot and legend caption
#' @export
#' @import ggplot2
setCaptionLinetype <- function(plotObject, linetype, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(linetype, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  linetypeExpression <- getCoreSetCaptionProperty("linetype")
  eval(linetypeExpression)

  return(newPlotObject)
}

#' @title setCaptionFill
#' @param plotObject \code{ggplot} graphical object
#' @param fill fills of the data in plot and legend caption
#' @param name reference names in caption$name identifying the legend caption lines
#' @return A \code{ggplot} graphical object
#' @description
#' Set the fills of the data in plot and legend caption
#' @export
#' @import ggplot2
setCaptionFill <- function(plotObject, fill, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsOfType(fill, c("character", "numeric"))

  # getCoreSetCaptionProperty is common for all the caption properties
  # prevent duplication of code everywhere
  fillExpression <- getCoreSetCaptionProperty("fill")
  eval(fillExpression)

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
setLegendTitle <- function(plotObject, title) {
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
setLegendPosition <- function(plotObject, position) {
  validateIsOfType(plotObject, "ggplot")
  validateIsIncluded(position, LegendPositions)

  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  legend <- newPlotObject$plotConfiguration$legend
  legend$position <- position

  legendPosition <- getLegendPosition(legend$position)

  newPlotObject <- newPlotObject + theme(
    legend.position = c(legendPosition$xPosition, legendPosition$yPosition),
    legend.justification = c(legendPosition$xJustification, legendPosition$yJustification),
    legend.direction = "vertical"
  )

  return(newPlotObject)
}

# LegendPositions needed to be defined before to tlfEnv$defaultLegendPosition
# It was consequently moved from utilities-legend to tlf-env

#' @title LegendTypes
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

mergeLegend <- function(plotObject, newLabels, color, shape, size, linetype, fill) {
  validateIsOfType(plotObject, "ggplot")

  oldCaption <- plotObject$plotConfiguration$legend$caption
  legendLength <- nrow(plotObject$plotConfiguration$legend$caption) %||% 0
  newCaption <- data.frame(
    name = newLabels, label = newLabels,
    visibility = rep(TRUE, length(newLabels)),
    order = seq(legendLength + 1, legendLength + length(newLabels)),
    color = color, shape = shape, size = size, linetype = linetype, fill = fill,
    stringsAsFactors = FALSE
  )

  mergeCaption <- rbind.data.frame(oldCaption, newCaption)
  plotObject <- setLegendCaption(plotObject, mergeCaption)
  plotObject <- setLegendPosition(plotObject, plotObject$plotConfiguration$legend$position)

  return(plotObject)
}

getCoreSetCaptionProperty <- function(property) {
  c(
    parse(text = 'validateIsOfType(plotObject, "ggplot")'),
    parse(text = "newCaption <- getLegendCaption(plotObject)"),
    parse(text = paste0(property, "Indices <- seq(1, length(", property, "))")),
    parse(text = paste0(
      "if(!is.null(name)){",
      "validateIsIncluded(name, newCaption$name) \n",
      "validateIsSameLength(", property, ", name) \n",
      property, "Indices <- match(name, newCaption$name)}"
    )),
    parse(text = paste0("newCaption$", property, "[", property, "Indices] <- ", property, " \n")),
    parse(text = "newPlotObject <- setLegendCaption(plotObject, newCaption)")
  )
}


getDefaultCaptionFor <- function(plotName) {
  as.data.frame(tlfEnv$currentTheme$defaultCaption[[plotName]], stringsAsFactors = FALSE)
}
