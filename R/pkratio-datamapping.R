#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
PKRatioDataMapping <- R6::R6Class("PKRatioDataMapping",
  inherit = XYDataMapping,
  public = list(
    colorGrouping = NULL,
    sizeGrouping = NULL,
    shapeGrouping = NULL,
      
    # Example of how to do some other stuff
    initialize = function(x = "Age", y="Ratio", colorGrouping=NULL, sizeGrouping = NULL, shapeGrouping = NULL) {
      self$colorGrouping <- colorGrouping
      self$sizeGrouping <- sizeGrouping
      self$shapeGrouping <- shapeGrouping
      super$initialize(x, y)
    }
  )
)


if (FALSE){
  # Example of definition of Data Mapping
  CmaxRatio.DataMapping <- list(
    "Axes" = list("X"="Age", "Y"=c("Ratio")),
    "Unit" = list("X"="yrs", "Y"=c(NULL)),
    "Grouping" = list("Color"= "Gender", "Symbol"=c("Compound", "Dose"))
  )
}