#' @title SetLegend
#' @param plotObject Graphical object created from ggplot
#' @param legendPosition element of legendPositions list
#' @param legendTitles Title of the legend captions
#' @param legendCaptions captions of the legend (as a list with LegendTypes attribute)
#' @param legendValues values of the legend aesthetic attributes
#' @return Current plot with Legend Layer
#' @description
#' SetLegend set a Legend Position and labels in a plot
#' @export
setLegend <- function(plotObject,
                      legendPosition,
                      legendTitles = NULL,
                      legendCaptions = NULL,
                      legendValues = NULL) {

  # Check Inputs
  validateIsOfType(plotObject, c("gg", "ggplot"))
  validateEnumValue(LegendPositions, legendPosition)

  legendTitles <- legendTitles %||% LegendTitles$new()
  legendCaptions <- legendCaptions %||% LegendCaptions$new()
  legendValues <- legendValues %||% LegendValues$new()

  validateIsOfType(legendTitles, "LegendTitles")
  validateIsOfType(legendCaptions, "LegendCaptions")
  validateIsOfType(legendValues, "LegendValues")

  plotObject <- setLegendPosition(plotObject = plotObject, legendPosition = legendPosition)

  # Redefine label of groups in legend
  for (legendType in LegendTypes) {
    plotObject <- plotObject + ifnotnull(
      legendCaptions[[legendType]],
      scale_discrete_manual(
        aesthetics = legendType,
        name = legendTitles[[legendType]],
        values = legendValues[[legendType]],
        labels = levels(as.factor(legendCaptions[[legendType]]))
      ),
      scale_discrete_manual(
        aesthetics = legendType,
        name = legendTitles[[legendType]],
        values = legendValues[[legendType]],
        labels = NA,
        guide = "none"
      )
    )
  }
  return(plotObject)
}

#' @title SetLegendPosition
#' @param plotObject Graphical object created from ggplot
#' @param legendPosition element of legendPositions list
#' @param Subplot.Index (optional) OPTION NOT IMPLEMENTED YET
#' @return Current plot with Legend Layer
#' @description
#' SetLegendPosition set a Legend Position as defined by legendPosition key
#' Default legend setting is within plot on the top right corner
#' @export
setLegendPosition <- function(plotObject,
                              legendPosition = LegendPositions$outsideRight,
                              Subplot.Index = NA) {
  # Check Plot Handle
  validateIsOfType(plotObject, c("gg", "ggplot"))

  # Check and Transform Legend Position enum into actual position
  legendPosition <- getLegendPosition(legendPosition %||% LegendPositions$none)

  plotHandle <- plotObject + theme(
    legend.position = c(legendPosition$xPosition, legendPosition$yPosition),
    legend.justification = c(legendPosition$xJustification, legendPosition$yJustification)
  )

  return(plotObject)
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

getLegendPosition <- function(position) {

  # Check if legend position is in enum
  validateEnumValue(LegendPositions, position)

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

# The following R6 classes inherit from Groupings
# Consequently, they get the very same color, shape, linetype... properties

LegendTitles <- R6::R6Class(
  "LegendTitles",
  inherit = GroupMapping,
  public = list(
    initialize = function(color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL,
                              groupings = NULL) {
      if (!is.null(groupings)) {
        self$color <- groupings$color$label
        self$fill <- groupings$fill$label
        self$linetype <- groupings$linetype$label
        self$shape <- groupings$shape$label
        self$size <- groupings$size$label
      } else {
        self$color <- color
        self$fill <- fill
        self$linetype <- linetype
        self$shape <- shape
        self$size <- size
      }
    }
  )
)

LegendCaptions <- R6::R6Class(
  "LegendCaptions",
  inherit = GroupMapping,
  public = list(
    initialize = function(color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL,
                              groupings = NULL,
                              data = NULL,
                              metaData = NULL) {
      if (!is.null(groupings) && !is.null(data)) {
        self$color <- ifnotnull(
          groupings$color$group,
          groupings$color$getCaptions(data, metaData),
          NA
        )
        self$fill <- ifnotnull(
          groupings$fill$group,
          groupings$fill$getCaptions(data, metaData),
          NA
        )
        self$linetype <- ifnotnull(
          groupings$linetype$group,
          groupings$linetype$getCaptions(data, metaData),
          NA
        )
        self$shape <- ifnotnull(
          groupings$shape$group,
          groupings$shape$getCaptions(data, metaData),
          NA
        )
        self$size <- ifnotnull(
          groupings$size$group,
          groupings$size$getCaptions(data, metaData),
          NA
        )
      } else {
        self$color <- color
        self$fill <- fill
        self$linetype <- linetype
        self$shape <- shape
        self$size <- size
      }
    }
  )
)

LegendValues <- R6::R6Class(
  "LegendValues",
  inherit = GroupMapping,
  public = list(
    initialize = function(aesProperties = tlfEnv$currentTheme$aesProperties,
                              color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL) {
      self$color <- color %||% aesProperties$color
      self$fill <- fill %||% aesProperties$fill
      self$linetype <- linetype %||% aesProperties$linetype
      self$shape <- shape %||% aesProperties$shape
      self$size <- size %||% aesProperties$size
    }
  )
)
