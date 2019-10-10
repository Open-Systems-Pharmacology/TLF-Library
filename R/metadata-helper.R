# Helper fonction to create a metaData variable
#' @title metaDataHelper
#' @param data data.frame
#' @return metaData with default OSP Suite units
#' @description
#' Create a metaData object with information about units and dimensions of data
#' @export
#' @examples
#' df <- pkRatioData
#' pkRatioMetaData <- metaDataHelper(data = df)
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
