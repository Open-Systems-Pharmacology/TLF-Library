#' @title plotPKRatio
#' @description
#' Producing PK Ratio plots
#'
#' @inheritParams addScatter
#' @param foldDistance Numeric values of fold distance lines to display in log plots.
#' This argument is internally translated into `lines` field of `dataMapping`.
#' @param dataMapping
#' A `PKRatioDataMapping` object mapping `x`, `y` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration
#' An optional `PKRatioPlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @references For examples, see:
#' <https://www.open-systems-pharmacology.org/TLF-Library/articles/pk-ratio-vignette.html>
#'
#' @export
#' @family molecule plots
#' @examples
#' # Produce PK Ratio plot
#' pkData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))
#'
#' plotPKRatio(data = pkData, dataMapping = PKRatioDataMapping$new(x = "x", y = "y"))
#'
#' # Produce PK Ratio plot with user-defined horizontal lines
#' plotPKRatio(
#'   data = pkData,
#'   dataMapping = PKRatioDataMapping$new(x = "x", y = "y"),
#'   foldDistance = c(1, 10)
#' )
#'
plotPKRatio <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        foldDistance = NULL,
                        plotObject = NULL) {
  eval(.parseCheckPlotInputs("PKRatio"))
  validateIsNumeric(foldDistance, nullAllowed = TRUE)
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- .getAesStringMapping(dataMapping)

  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  # Include horizontal lines
  if (!isEmpty(foldDistance)) {
    dataMapping$lines <- getLinesFromFoldDistance(foldDistance)
  }
  for (lineIndex in seq_along(dataMapping$lines)) {
    # position correspond to the number of layer lines already added
    eval(.parseAddLineLayer("horizontal", dataMapping$lines[[lineIndex]], lineIndex - 1))
  }

  # If uncertainty is defined, add error bars
  if (!isOfLength(dataMapping$uncertainty, 0)) {
    eval(.parseAddUncertaintyLayer())
  }

  eval(.parseAddScatterLayer())
  # Define shapes and colors based on plotConfiguration$points properties
  eval(.parseUpdateAestheticProperty(AestheticProperties$color, "points"))
  eval(.parseUpdateAestheticProperty(AestheticProperties$shape, "points"))
  eval(.parseUpdateAxes())
  return(plotObject)
}
