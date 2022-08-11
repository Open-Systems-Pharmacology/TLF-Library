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
  #----- Validation and formatting of input arguments -----
  if (all(isEmpty(data), isEmpty(observedData))) {
    stop("At least 'data' or 'observedData' is required.")
  }
  validateIsOfType(data, "data.frame", nullAllowed = TRUE)
  validateIsOfType(observedData, "data.frame", nullAllowed = TRUE)

  if (!isEmpty(data)) {
    dataMapping <- .setDataMapping(dataMapping, TimeProfileDataMapping, data)
  }
  if (!isEmpty(observedData)) {
    observedDataMapping <- .setDataMapping(observedDataMapping, ObservedDataMapping, observedData)
  }
  # If data is empty, plotConfiguration from observedData is used
  if (isEmpty(data)) {
    plotConfiguration <- .setPlotConfiguration(
      plotConfiguration, TimeProfilePlotConfiguration,
      observedData, metaData, observedDataMapping
    )
  }
  plotConfiguration <- .setPlotConfiguration(
    plotConfiguration, TimeProfilePlotConfiguration,
    data, metaData, dataMapping
  )

  plotObject <- .setPlotObject(plotObject, plotConfiguration)

  #----- Build layers of molecule plot -----
  #--- Simulated data ---
  if (!isEmpty(data)) {
    mapData <- dataMapping$checkMapData(data)
    mapLabels <- .getAesStringMapping(dataMapping)

    # Add ribbons for population time profiles
    if (!any(isEmpty(dataMapping$ymin), isEmpty(dataMapping$ymax))) {
      aestheticValues <- .getAestheticValuesFromConfiguration(
        n = 1,
        position = 0,
        plotConfigurationProperty = plotObject$plotConfiguration$ribbons,
        propertyNames = c("alpha")
      )
      plotObject <- plotObject +
        ggplot2::geom_ribbon(
          data = mapData,
          mapping = ggplot2::aes_string(
            x = mapLabels$x,
            ymin = mapLabels$ymin,
            ymax = mapLabels$ymax,
            fill = mapLabels$fill,
            group = mapLabels$linetype
          ),
          alpha = aestheticValues$alpha,
          na.rm = TRUE,
          show.legend = TRUE
        )
    }
    # Add simulated time profile
    if (!isEmpty(dataMapping$y)) {
      aestheticValues <- .getAestheticValuesFromConfiguration(
        n = 1,
        position = 0,
        plotConfigurationProperty = plotObject$plotConfiguration$lines,
        propertyNames = c("alpha", "size")
      )
      plotObject <- plotObject +
        ggplot2::geom_path(
          data = mapData,
          mapping = ggplot2::aes_string(
            x = mapLabels$x,
            y = mapLabels$y,
            color = mapLabels$color,
            linetype = mapLabels$linetype
          ),
          size = aestheticValues$size,
          alpha = aestheticValues$alpha,
          na.rm = TRUE,
          show.legend = TRUE,
        )
    }

    #----- Update properties using ggplot2::scale functions -----
    plotObject <- .updateAesProperties(
      plotObject,
      plotConfigurationProperty = "ribbons",
      propertyNames = "fill",
      data = mapData,
      mapLabels = mapLabels
    )
    plotObject <- .updateAesProperties(
      plotObject,
      plotConfigurationProperty = "lines",
      propertyNames = "linetype",
      data = mapData,
      mapLabels = mapLabels
    )

    # Get aesthetic information of simulated data for creating a legend caption data.frame
    simColumnNames <- .getAesPropertyColumnNameFromLabels(
      mapLabels,
      c("color", "fill", "linetype")
    )
    simAesLengths <- .getAesPropertyLengthFromLabels(
      data = mapData,
      simColumnNames,
      c("color", "fill", "linetype")
    )

    simFillValues <- .getAestheticValuesFromConfiguration(
      n = simAesLengths$fill,
      plotConfigurationProperty = plotObject$plotConfiguration$ribbons,
      propertyNames = "fill"
    )$fill
    simLinetypeValues <- .getAestheticValuesFromConfiguration(
      n = simAesLengths$linetype,
      plotConfigurationProperty = plotObject$plotConfiguration$lines,
      propertyNames = "linetype"
    )$linetype
    simColorValues <- .getAestheticValuesFromConfiguration(
      n = simAesLengths$color,
      plotConfigurationProperty = plotObject$plotConfiguration$lines,
      propertyNames = "color"
    )$color
  }

  #--- Simulated data only ---
  if (isEmpty(observedData)) {
    plotObject <- .updateAesProperties(
      plotObject,
      plotConfigurationProperty = "lines",
      propertyNames = "color",
      data = mapData,
      mapLabels = mapLabels
    )

    # Add a legend caption list if legend or properties need to be updated
    plotObject$plotConfiguration$legend$caption <- list(
      color = data.frame(
        names = levels(mapData[, simColumnNames$color]),
        labels = levels(mapData[, simColumnNames$color]),
        values = simColorValues,
        stringsAsFactors = FALSE
      ),
      fill = data.frame(
        names = levels(mapData[, simColumnNames$fill]),
        labels = levels(mapData[, simColumnNames$fill]),
        colorNames = .getColorNamesForFirstAesValues(mapData, simColumnNames, "fill"),
        values = simFillValues %||% NA,
        stringsAsFactors = FALSE
      ),
      linetype = data.frame(
        names = levels(mapData[, simColumnNames$linetype]),
        labels = levels(mapData[, simColumnNames$linetype]),
        colorNames = .getColorNamesForFirstAesValues(mapData, simColumnNames, "linetype"),
        values = simLinetypeValues %||% "blank",
        stringsAsFactors = FALSE
      ),
      shape = data.frame(
        names = levels(mapData[, simColumnNames$color]),
        labels = levels(mapData[, simColumnNames$color]),
        colorNames = levels(mapData[, simColumnNames$color]),
        values = " ",
        stringsAsFactors = FALSE
      )
    )

    plotObject <- .updateAxes(plotObject)
    return(plotObject)
  }

  #--- Observed data ---
  # Then, add observed data
  mapObservedData <- observedDataMapping$checkMapData(observedData)
  observedMapLabels <- .getAesStringMapping(observedDataMapping)

  if (!any(isEmpty(observedDataMapping$ymin), isEmpty(observedDataMapping$ymax))) {
    plotObject <- .addErrorbarLayer(
      plotObject,
      data = mapObservedData,
      mapLabels = observedMapLabels
    )
  }

  plotObject <- .addScatterLayer(
    plotObject,
    data = mapObservedData,
    mapLabels = observedMapLabels
  )

  plotObject <- .updateAesProperties(
    plotObject,
    plotConfigurationProperty = "points",
    propertyNames = "shape",
    data = mapObservedData,
    mapLabels = observedMapLabels
  )

  obsColumnNames <- .getAesPropertyColumnNameFromLabels(
    observedMapLabels,
    c("color", "shape")
  )
  obsAesLengths <- .getAesPropertyLengthFromLabels(
    data = mapObservedData,
    obsColumnNames,
    c("color", "shape")
  )

  obsShapeValues <- .getAestheticValuesFromConfiguration(
    n = obsAesLengths$shape,
    plotConfigurationProperty = plotObject$plotConfiguration$points,
    propertyNames = "shape"
  )$shape
  obsColorValues <- .getAestheticValuesFromConfiguration(
    n = obsAesLengths$color,
    plotConfigurationProperty = plotObject$plotConfiguration$points,
    propertyNames = "color"
  )$color

  #--- Observed data only ---
  if (isEmpty(data)) {
    plotObject <- .updateAesProperties(
      plotObject,
      plotConfigurationProperty = "points",
      propertyNames = "color",
      data = mapObservedData,
      mapLabels = observedMapLabels
    )

    plotObject$plotConfiguration$legend$caption <- list(
      color = data.frame(
        names = levels(mapObservedData[, obsColumnNames$color]),
        labels = levels(mapObservedData[, obsColumnNames$color]),
        values = obsColorValues,
        stringsAsFactors = FALSE
      ),
      fill = data.frame(
        names = levels(mapObservedData[, obsColumnNames$color]),
        labels = levels(mapObservedData[, obsColumnNames$color]),
        colorNames = levels(mapObservedData[, obsColumnNames$color]),
        values = NA,
        stringsAsFactors = FALSE
      ),
      linetype = data.frame(
        names = levels(mapObservedData[, obsColumnNames$color]),
        labels = levels(mapObservedData[, obsColumnNames$color]),
        colorNames = levels(mapObservedData[, obsColumnNames$color]),
        values = "blank",
        stringsAsFactors = FALSE
      ),
      shape = data.frame(
        names = levels(mapObservedData[, obsColumnNames$shape]),
        labels = levels(mapObservedData[, obsColumnNames$shape]),
        colorNames = .getColorNamesForFirstAesValues(mapObservedData, obsColumnNames, "shape"),
        values = obsShapeValues %||% NA,
        stringsAsFactors = FALSE
      )
    )

    plotObject <- .updateAxes(plotObject)
    return(plotObject)
  }

  #--- Simulated and observed data ---
  # The final color vector needs a length of totalLength to prevent scale_color_manual to crash
  colorBreaks <- levels(c(mapData[, simColumnNames$color], mapObservedData[, obsColumnNames$color]))
  totalColorLength <- length(colorBreaks)

  # colorValues are selected colors for simulated (and shared observed data) and then colors for remaining observed data
  # the function ".getAestheticValues" selects these values as defined in the plotConfiguration object
  finalColorValues <- c(
    .getAestheticValues(
      n = simAesLengths$color,
      selectionKey = plotConfiguration$lines$color,
      aesthetic = "color"
    ),
    # By using position = simAesLengths$color,
    # the function will start selecting the colors that come after the colors selected for lines
    # this aims at preventing a reset of the colors and a need for manual update of the user
    .getAestheticValues(
      n = totalColorLength - simAesLengths$color,
      selectionKey = plotConfiguration$points$color,
      position = simAesLengths$color,
      aesthetic = "color"
    )
  )

  # Export the legend captions so the user can update legend keys order
  plotObject$plotConfiguration$legend$caption <- list(
    color = data.frame(
      names = colorBreaks,
      labels = colorBreaks,
      values = finalColorValues,
      stringsAsFactors = FALSE
    ),
    fill = data.frame(
      names = levels(mapData[, simColumnNames$fill]),
      labels = levels(mapData[, simColumnNames$fill]),
      colorNames = .getColorNamesForFirstAesValues(mapData, simColumnNames, "fill"),
      values = simFillValues %||% NA,
      stringsAsFactors = FALSE
    ),
    linetype = data.frame(
      names = levels(mapData[, simColumnNames$linetype]),
      labels = levels(mapData[, simColumnNames$linetype]),
      colorNames = .getColorNamesForFirstAesValues(mapData, simColumnNames, "linetype"),
      values = simLinetypeValues %||% "blank",
      stringsAsFactors = FALSE
    ),
    shape = data.frame(
      names = levels(mapObservedData[, obsColumnNames$shape]),
      labels = levels(mapObservedData[, obsColumnNames$shape]),
      colorNames = .getColorNamesForFirstAesValues(mapObservedData, obsColumnNames, "shape"),
      values = obsShapeValues %||% NA,
      stringsAsFactors = FALSE
    )
  )

  plotObject <- updateTimeProfileLegend(
    plotObject = plotObject,
    caption = plotObject$plotConfiguration$legend$caption
  )

  # remove the legend of aesthetic if default unmapped aesthetic
  if (isIncluded(simColumnNames$color, "legendLabels") & isIncluded(obsColumnNames$color, "legendLabels")) {
    plotObject <- plotObject + ggplot2::guides(color = "none")
  }
  plotObject <- .updateAxes(plotObject)
  return(plotObject)
}


#' @title updateTimeProfileLegend
#' @description Update time profile legend caption
#' @param plotObject A ggplot object
#' @param caption A data.frame as obtained from `getLegendCaption` to use for updating a plot legend.
#' @return A `ggplot` object
#' @export
updateTimeProfileLegend <- function(plotObject, caption) {
  # Apply ggplot2 scale functions only to defined/mapped aesthetics
  # values for undefined aesthetics are set to blank symbol, linetype, etc.
  blankValues <- list(
    shape = " ",
    linetype = "blank",
    fill = NA
  )
  scaleFunction <- list(
    shape = ggplot2::scale_shape_manual,
    linetype = ggplot2::scale_linetype_manual,
    fill = ggplot2::scale_fill_manual
  )

  unDefinedNames <- list()
  for (property in c("shape", "linetype", "fill")) {
    # Use suppressMessages to prevent ggplot2 to
    # throw message that scale was updated
    suppressMessages(
      plotObject <- plotObject +
        scaleFunction[[property]](
          breaks = caption[[property]]$names,
          labels = caption[[property]]$labels,
          values = caption[[property]]$values
        )
    )
    # Get legend names that are not scaled but in final legend
    unDefinedNames[[property]] <- setdiff(
      caption$color$names,
      caption[[property]]$colorNames
    )
    # Fill in the caption guide for shape, linetype and fill
    caption[[property]] <- rbind.data.frame(
      caption[[property]],
      data.frame(
        names = unDefinedNames[[property]],
        labels = unDefinedNames[[property]],
        colorNames = unDefinedNames[[property]],
        values = rep(blankValues[[property]], length(unDefinedNames[[property]]))
      ),
      stringsAsFactors = FALSE
    )
    # Keep only names and order provided by color legend
    captionOrder <- sapply(
      caption$color$names,
      function(colorName) {
        head(which(caption[[property]]$colorNames == colorName), 1)
      }
    )
    caption[[property]] <- caption[[property]][captionOrder, ]
  }

  # Update color scale and use color caption
  # as reference for displaying final legend
  suppressMessages(
    plotObject <- plotObject +
      ggplot2::scale_color_manual(
        breaks = caption$color$names,
        labels = caption$color$labels,
        values = caption$color$values
      ) +
      ggplot2::guides(
        fill = "none", shape = "none", linetype = "none",
        color = ggplot2::guide_legend(
          title = plotObject$plotConfiguration$legend$title$text,
          title.theme = plotObject$plotConfiguration$legend$title$createPlotFont(),
          override.aes = list(
            shape = caption$shape$values,
            fill = caption$fill$values,
            linetype = caption$linetype$values
          )
        )
      )
  )
  plotObject$plotConfiguration$legend$caption <- caption
  return(plotObject)
}
