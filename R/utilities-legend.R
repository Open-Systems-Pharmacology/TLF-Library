#' @title SetLegend
#' @param plotHandle Graphical object created from ggplot
#' @param legendPosition element of legendPositions list
#' @param legendColorCaptions captions of the legend
#' @return Current plot with Legend Layer
#' @description
#' SetLegend set a Legend Position and labels in a plot
#' @export
setLegend <- function(plotHandle,
                      legendPosition,
                      legendColorCaptions = NULL,
                      legendSizeCaptions = NULL,
                      legendShapeCaptions = NULL) {

  # Check Inputs
  stopifnot(class(plotHandle) %in% c("gg", "ggplot"))

  plotHandle <- setLegendPosition(plotHandle = plotHandle, legendPosition = legendPosition)

  # Redefine label of groups in legend
  plotHandle <- plotHandle +
    ifnotnull(legendColorCaptions, scale_color_discrete(legendColorCaptions)) +
    ifnotnull(legendSizeCaptions, scale_size_discrete(legendSizeCaptions)) +
    ifnotnull(legendShapeCaptions, scale_shape_discrete(legendShapeCaptions))

  return(plotHandle)
}

#' @title SetLegendPosition
#' @param plotHandle Graphical object created from ggplot
#' @param legendPosition element of legendPositions list
#' @param Subplot.Index (optional) OPTION NOT IMPLEMENTED YET
#' @return Current plot with Legend Layer
#' @description
#' SetLegendPosition set a Legend Position as defined by legendPosition key
#' Default legend setting is within plot on the top right corner
#' @export
setLegendPosition <- function(plotHandle,
                              legendPosition = legendPositions$outsideRight,
                              Subplot.Index = NA) {
  # Check Plot Handle
  stopifnot(class(plotHandle) %in% c("gg", "ggplot"))

  # Check Legend Position is correct
  legendPosition <- legendPosition %||% "none"
  if ("character" %in% class(legendPosition)) {
    stopifnot(legendPosition %in% names(legendPositions))
    legendPosition <- legendPositions[[legendPosition]]
  }

  plotHandle <- plotHandle + theme(
    legend.position = c(legendPosition$xPosition, legendPosition$yPosition),
    legend.justification = c(legendPosition$xJustification, legendPosition$yJustification)
  )

  return(plotHandle)
}

# Function to get the list of available legend positions
getLegendPositions <- function(which = NULL) {
  positionNames <- names(legendPositions)

  if (!is.null(which)) {
    stopifnot(is.numeric(which))
    positionNames <- positionNames[which]
  }
  return(positionNames)
}

# Enum of all possible combinations of legend positions with their specifications
legendPositions <- list(
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
