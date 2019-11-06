# Helper fonction to create a metaData variable
#' @title metaDataHelper
#' @param data data.frame
#' @return metaData with default OSP Suite units
#' @description
#' Create a metaData object with information about units and dimensions of data
#' @export
metaDataHelper <- function(data) {
  metaData <- enum(names(data))
  for (variable in colnames(data)) {
    metaData[[variable]] <- list("unit" = "", "dimension" = "", "variableType" = "")

    if (variable %in% "Age") {
      metaData[[variable]] <- list("unit" = "yrs", "dimension" = "age", "variableType" = "numeric")
    }
    if (variable %in% "Time") {
      metaData[[variable]] <- list("unit" = "min", "dimension" = "time", "variableType" = "numeric")
    }
    if (variable %in% "Dose") {
      metaData[[variable]] <- list("unit" = "mg", "dimension" = "amount", "variableType" = "numeric")
    }
    if (variable %in% c("Observed", "Simulated", "Value")) {
      metaData[[variable]] <- list("unit" = "mumol/L", "dimension" = "concentration", "variableType" = "numeric")
    }
    if (variable %in% c("Ratio")) {
      metaData[[variable]] <- list("unit" = "", "dimension" = "ratio", "variableType" = "numeric")
    }
  }
  return(metaData)
}

metaDataAlternativeFormat <- function(data) {
  metaData <- data.frame(unit = rep("", length(names(data))),
                         dimension = rep("", length(names(data))),
                         LLOQ = rep(NA,length(names(data))),
                         row.names = names(data))
  
  metaData$unit <- as.character(metaData$unit)
  metaData$dimension <- as.character(metaData$dimension)
  metaData$LLOQ <- as.character(metaData$LLOQ)
 
  for (variable in colnames(data)) {
    if (variable %in% "Age") {
      metaData[variable, c("unit", "dimension")] <- c("yrs", "Age")
    }
    if (variable %in% "Time") {
      metaData[variable, c("unit", "dimension")] <- c("min", "Time")
    }
    if (variable %in% "Dose") {
      metaData[variable, c("unit", "dimension")] <- c("mg", "Amount")
    }
    if (variable %in% c("Observed", "Simulated", "Value")) {
      metaData[variable, c("unit", "dimension")] <- c("umol/L", "Concentration")
    }
    if (variable %in% c("Ratio")) {
      metaData[variable, c("unit", "dimension")] <- c("", "Fraction")
    }
  }
  return(metaData) 
}