#' @title SetLegendPosition
#' @param plotHandle Graphical object created from ggplot
#' @param plotConfiguration plot Configuration for legend
#' @param dataMapping mapping (might be overwritten to use plotConfiguration instead)
#' @return Current plot with Legend Layer
#' @description
#' SetLegend set a Legend Position and labels in a plot
#' @export
setLegend <- function(plotHandle, plotConfiguration, dataMapping) {

  # Check Inputs
  stopifnot(class(plotHandle) %in% c("gg", "ggplot"))

  # Set Legend Position according to plotConfiguration
  # plotConfiguration will be added when implemented
  plotHandle <- setLegendPosition(plotHandle)

  # Redefine label of groups in legend
  if (is.null(dataMapping$colorGrouping)) {
    plotHandle <- plotHandle + guides(color = "none")
  } else {
    plotHandle <- plotHandle + guides(color = guide_legend(data$colorGrouping))
  }
  if (is.null(dataMapping$sizeGrouping)) {
    plotHandle <- plotHandle + guides(size = "none")
  } else {
    plotHandle <- plotHandle + guides(size = guide_legend(data$sizeGrouping))
  }
  if (is.null(dataMapping$shapeGrouping)) {
    plotHandle <- plotHandle + guides(shape = "none")
  } else {
    plotHandle <- plotHandle + guides(shape = guide_legend(data$shapeGrouping))
  }

  return(plotHandle)
}

#' @title SetLegendPosition
#' @param plotHandle Graphical object created from ggplot
#' @param Location Inside/Oustide/None
#' @param X.Location NA/Left/Center/Right
#' @param Y.Location NA/Top/Center/Bottom
#' @param Subplot.Index (optional) OPTION NOT IMPLEMENTED YET
#' @return Current plot with Legend Layer
#' @description
#' SetLegendPosition set a Legend Position in a plot (case insensitive)
#' Default legend setting is within plot on the top right corner
#' @export
setLegendPosition <- function(plotHandle,
                              Location = "inside",
                              X.Location = NA,
                              Y.Location = NA,
                              Subplot.Index = NA) {
  # Check Plot Handle
  stopifnot(class(plotHandle) %in% c("gg", "ggplot"))

  # Check Location, if NULL will hide legend
  if (is.null(Location) || tolower(Location) %in% "none") {
    plotHandle <- plotHandle + theme(legend.position = "none")
  }
  else {
    # If legend XY locations are set as null, place them on the top right side of the plot
    if (is.null(X.Location)) {
      X.Location <- NA
    }
    if (is.null(Y.Location)) {
      Y.Location <- NA
    }
    # Check that Locations are correctly input
    stopifnot(
      tolower(Location) %in% c("inside", "outside"),
      tolower(X.Location) %in% c(NA, "left", "center", "middle", "right"),
      tolower(Y.Location) %in% c(NA, "top", "center", "middle", "bottom")
    )

    if (tolower(Location) %in% "inside") {
      # Convert Position Label into inside position
      if (tolower(X.Location) %in% "left") {
        X.Location <- 0.025
        X.Justification <- 0
      }
      if (tolower(X.Location) %in% c("center", "middle")) {
        X.Location <- 0.5
        X.Justification <- 0.5
      }
      if (tolower(X.Location) %in% c(NA, "right")) {
        X.Location <- 0.975
        X.Justification <- 1
      }
      if (tolower(Y.Location) %in% c(NA, "top")) {
        Y.Location <- 0.975
        Y.Justification <- 1
      }
      if (tolower(Y.Location) %in% c("center", "middle")) {
        Y.Location <- 0.5
        Y.Justification <- 0.5
      }
      if (tolower(Y.Location) %in% "bottom") {
        Y.Location <- 0.025
        Y.Justification <- 0
      }

      plotHandle <- plotHandle + theme(
        legend.position = c(X.Location, Y.Location),
        legend.justification = c(X.Justification, Y.Justification)
      )
    }
    if (tolower(Location) %in% "outside") {
      # As outside reference, choose to add legend on top of plot centered
      if (is.na(X.Location) && is.na(Y.Location)) {
        plotHandle <- plotHandle + theme(legend.position = "top")
      }
      if (tolower(X.Location) %in% c(NA, "center", "middle") && !tolower(Y.Location) %in% c(NA, "center", "middle")) {
        plotHandle <- plotHandle + theme(legend.position = tolower(Y.Location))
      }
      if (tolower(Y.Location) %in% c(NA, "center", "middle") && !tolower(X.Location) %in% c(NA, "center", "middle")) {
        plotHandle <- plotHandle + theme(legend.position = tolower(X.Location))
      }
      if (!tolower(X.Location) %in% c(NA, "center", "middle") && !tolower(Y.Location) %in% c(NA, "center", "middle")) {
        # Prioritize Y.Location (top/bottom) and justified through X.Location
        if (tolower(X.Location) %in% "left") {
          X.Location <- 0
        }
        if (tolower(X.Location) %in% "right") {
          X.Location <- 1
        }
        plotHandle <- plotHandle + theme(
          legend.position = tolower(Y.Location),
          legend.justification = c(X.Location, 0.5)
        )
      }
    }
  }
  return(plotHandle)
}
