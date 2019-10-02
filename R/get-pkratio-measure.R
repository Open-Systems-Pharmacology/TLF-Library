#' get PK-Ratio qualification measure
#'
#' @title getPKRatioMeasure
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param metaData list of lists (structure TO BE DISCUSSED)
#' containing complementary information to data (e.g. unit)
#' @param dataMapping R6 class PKRatioDataMapping
#' mapping of x, y axes + mapping of colorGrouping, sizeGrouping, shapeGrouping
#' @description
#' getPKRatioMeasure(data, metaData, dataMapping)
#' @return a data.frame reporting the PK Ratio Qualification Measure
#' @export
#'
getPKRatioMeasure <- function(data, dataMapping = NULL, ratioLimits = c(1.5, 2)) {
  # If no data mapping is input, use default
  dataMapping <- dataMapping %||% PKRatioDataMapping$new()

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))

  y <- dataMapping$y

  # Remove NA values
  PKratios <- data[!is.na.data.frame(data[, y]), y]

  PointsTotal <- length(PKratios)

  PKRatioMeasure <- data.frame("Number" = PointsTotal, "Ratio" = NA, row.names = "Points Total")

  for (limit in ratioLimits) {
    PointsWithin <- sum(PKratios <= limit & PKratios >= 1 / limit)
    PKRatioMeasureLim <- data.frame("Number" = PointsWithin, "Ratio" = PointsWithin / PointsTotal, row.names = paste("Points within ", limit, "-fold", sep = ""))

    PKRatioMeasure <- rbind.data.frame(PKRatioMeasure, PKRatioMeasureLim)
  }

  return(PKRatioMeasure)
}
