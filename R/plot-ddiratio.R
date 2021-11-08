#' @title plotDDIRatio
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping A `DDIRatioDataMapping` object
#' @param plotConfiguration A `DDIRatioPlotConfiguration` object
#' @param plotObject `ggplot` graphical object to which the PK Ratio plot layer is added
#' This parameter is optional: the `tlf` library will initialize an empty plot if the parameter is NULL or not provided
#' @param residualsVsObserved if `TRUE`, lines of constant residuals are drawn horizontally, as for a residuals vs observations
#' DDI ratio plot.  Otherwise, these lines are drawn diagonally, as for a predictions vs observations DDI ratio plot.
#' @description
#' Add DDI Ratio plot layers to a `ggplot` graphical object.
#' Inclcuding Guest et al. limits, DDI Ratio limits as diagonal lines and
#' DDI Ratios as a scatter plot.
#' @return A `ggplot` graphical object
#' @export
plotDDIRatio <- function(data,
                         metaData = NULL,
                         dataMapping = NULL,
                         plotConfiguration = NULL,
                         plotObject = NULL,
                         residualsVsObserved = NULL) {
  eval(parseCheckPlotInputs("DDIRatio"))
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)


  residualsVsObserved <- residualsVsObserved %||% dataMapping$residualsVsObserved %||% FALSE
  validateIsLogical(residualsVsObserved, nullAllowed = FALSE)

  lineOrientation <- "diagonal"
  if(residualsVsObserved){
    lineOrientation <- "ddiHorizontal"
  }
  # Include horizontal lines
  for (lineIndex in seq_along(dataMapping$lines)) {
    # position correspond to the number of layer lines already added
    eval(parseAddLineLayer(lineOrientation, dataMapping$lines[[lineIndex]], lineIndex - 1))
  }
  if (isOfLength(lineIndex, 0)) {
    lineIndex <- 0
  }
  # Add Guest et al. lines to plot
  # guestData is a data.frame with x, ymin and ymax
  guestData <- getGuestValuesFromDataMapping(data, dataMapping)
  plotObject <- plotObject +
    ggplot2::geom_path(
      data = guestData,
      mapping = ggplot2::aes_string(x = "x", y = "ymin"),
      na.rm = TRUE,
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
    ) +
    ggplot2::geom_path(
      data = guestData,
      mapping = ggplot2::aes_string(x = "x", y = "ymax"),
      na.rm = TRUE,
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
    )

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    eval(parseAddUncertaintyLayer())
  }
  eval(parseAddScatterLayer())
  # Define shapes and colors based on plotConfiguration$points properties
  eval(parseUpdateAestheticProperty(AestheticProperties$color, "points"))
  eval(parseUpdateAestheticProperty(AestheticProperties$shape, "points"))
  plotObject <- setLegendPosition(plotObject)
  plotObject <- setLegendFont(plotObject)
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @title getGuestValuesFromDataMapping
#' @description Get a data.frame with Guest et al. ratio limits
#' @param data data.frame containing the data to be used for the plot
#' @param dataMapping A `DDIRatioDataMapping` object
#' @return A data.frame with x, ymin and ymax defining Guest et al. limits
#' @export
getGuestValuesFromDataMapping <- function(data,
                                          dataMapping) {
  # Create vector of x values
  xData <- data[, dataMapping$x]
  xmin <- min(dataMapping$minRange, xData[xData > 0])
  xmax <- max(dataMapping$minRange, xData[xData > 0])
  # By default, use 500 points to get enough discretization for the plot
  x <- 10^(seq(log10(xmin), log10(xmax), length.out = 5e2))
  return(getGuestValues(x, delta = dataMapping$deltaGuest, residualsVsObserved = dataMapping$residualsVsObserved))
}

#' @title getGuestValues
#' @description Get a data.frame with Guest et al. ratio limits
#' @param x input values of Guest function
#' @param delta parameter of Guest function
#' @param residualsVsObserved if true, returns a dataframe for a residuals (predicted/observed) vs observed DDI ratio plot
#' @return A data.frame with x, ymin and ymax defining Guest et al. limits
#' @export
getGuestValues <- function(x, delta = 1, residualsVsObserved = FALSE) {
  xSym <- x
  xSym[x < 1] <- 1 / x[x < 1]
  limit <- (delta + 2 * (xSym - 1)) / xSym
  ymin <- x / limit
  ymax <- x * limit

  if(residualsVsObserved){
    guestLines <- data.frame(x = x, ymin =1/limit, ymax = limit)
    return(guestLines)
  }
  guestLines <- data.frame(x = x, ymin = ymin, ymax = ymax)
  return(guestLines)
}
