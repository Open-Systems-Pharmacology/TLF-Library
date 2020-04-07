#' get PK-Ratio qualification measure
#'
#' @title getPKRatioMeasure
#' @param data data.frame (or list of data.frames? TO BE DISCUSSED)
#' containing the data to be used for the plot
#' @param dataMapping R6 class PKRatioDataMapping
#' mapping of x, y and groupings
#' @param ratioLimits vector of numeric
#' containing the fold limits of PK ratios
#' As default limits are 1.5 and 2 folds
#' @description
#' getPKRatioMeasure(data, dataMapping, ratioLimits) get the number of PK ratios
#' that are within specific limits
#' @return a data.frame reporting the PK Ratio Qualification Measure
#' @export
#'
getPKRatioMeasure <- function(data, dataMapping = NULL, ratioLimits = c(1.5, 2)) {
  
  if(is.numeric(data)){
    PKratios <- data[!is.na(data)]
  }else{
  # If no data mapping is input, use default
  dataMapping <- dataMapping %||% PKRatioDataMapping$new(data = data)

  stopifnot("PKRatioDataMapping" %in% class(dataMapping))

  y <- dataMapping$y
  
  # Remove NA values
  PKratios <- data[!is.na.data.frame(data[, y]), y]}

  PointsTotal <- length(PKratios)

  PKRatioMeasure <- data.frame("Number" = PointsTotal, "Ratio" = NA, row.names = "Points Total")

  for (limit in ratioLimits) {
    PointsWithin <- sum(PKratios <= limit & PKratios >= 1 / limit)
    PKRatioMeasureLim <- data.frame("Number" = PointsWithin, "Ratio" = PointsWithin / PointsTotal, row.names = paste("Points within ", limit, "-fold", sep = ""))

    PKRatioMeasure <- rbind.data.frame(PKRatioMeasure, PKRatioMeasureLim)
  }

  return(PKRatioMeasure)
}
