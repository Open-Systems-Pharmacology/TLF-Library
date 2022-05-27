#' @title plotDDIRatio
#' @description
#' Producing DDI Ratio plots
#'
#' @inheritParams addScatter
#' @param residualsVsObserved Optional logical value defining
#' if DDI Ratio plot is drawn as residuals vs observed, instead of predicted vs observed.
#' @param dataMapping
#' A `DDIRatioDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration
#' An optional `DDIRatioPlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @references For examples, see:
#' <https://www.open-systems-pharmacology.org/TLF-Library/articles/pk-ratio-vignette.html>
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce DDI Ratio plot
#' ddiData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#'
#' plotDDIRatio(data = ddiData, dataMapping = DDIRatioDataMapping$new(x = "x", y = "y"))
#'
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
  if (residualsVsObserved) {
    lineOrientation <- "ddiHorizontal"
  }
  # Include diagonal or horizontal lines depending on the plot type
  for (lineIndex in seq_along(dataMapping$lines)) {
    # position correspond to the number of layer lines already added
    eval(parseAddLineLayer(lineOrientation, dataMapping$lines[[lineIndex]], lineIndex - 1))
  }
  if (isEmpty(lineIndex)) {
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
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = lineIndex, aesthetic = "size")
    ) +
    ggplot2::geom_path(
      data = guestData,
      mapping = ggplot2::aes_string(x = "x", y = "ymax"),
      na.rm = TRUE,
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$color, position = lineIndex, aesthetic = "color"),
      linetype = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$linetype, position = lineIndex, aesthetic = "linetype"),
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$alpha, position = lineIndex, aesthetic = "alpha"),
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
  eval(parseUpdateAxes())
  return(plotObject)
}

#' @title getGuestValuesFromDataMapping
#' @description
#' Get a data.frame with Guest et al. ratio limits from `data` and its `DDIRatioDataMapping`
#' @inheritParams plotDDIRatio
#' @return A data.frame with `x`, `ymin` and `ymax` defining Guest et al. limits
#' @export
#' @examples
#' # Get the data.frame of Guest et al. limits
#' ddiData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#'
#' getGuestValuesFromDataMapping(
#'   data = ddiData,
#'   dataMapping = DDIRatioDataMapping$new(x = "x", y = "y")
#' )
#'
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
#' @description
#' Get a data.frame with Guest et al. ratio limits with:
#' \itemize{
#' \item `ymax` = `x`.`limit`
#' \item `ymin` = `x`/`limit`
#' \item `limit` = (`delta`+2(`x`-1))/`x`
#' }
#'
#' @param x Numeric values input of Guest function
#' @param delta Numeric value parameter of Guest function
#' @param residualsVsObserved Logical value defining
#' if limits are claculated as residuals vs observed, instead of predicted vs observed.
#' @return A data.frame with `x`, `ymin` and `ymax` defining Guest et al. limits
#' @references
#' <https://dmd.aspetjournals.org/content/39/2/170>
#' @export
#' @examples
#' # Get predicted vs observed Guest et al. limits
#' getGuestValues(x = 10^seq(-2, 2, 0.2))
#'
#' # Get residuals vs observed Guest et al. limits
#' getGuestValues(x = 10^seq(-2, 2, 0.2), residualsVsObserved = TRUE)
#'
getGuestValues <- function(x, delta = 1, residualsVsObserved = FALSE) {
  xSym <- x
  xSym[x < 1] <- 1 / x[x < 1]
  limit <- (delta + 2 * (xSym - 1)) / xSym
  ymin <- x / limit
  ymax <- x * limit

  if (residualsVsObserved) {
    guestLines <- data.frame(x = x, ymin = 1 / limit, ymax = limit)
    return(guestLines)
  }
  guestLines <- data.frame(x = x, ymin = ymin, ymax = ymax)
  return(guestLines)
}
