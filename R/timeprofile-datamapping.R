#' @title TimeProfileDataMapping
#' @docType class
#' @description  Data Mapping for Time Profile
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = XYDataMapping,
  public = list(
    x = NULL,
    y = NULL,
    error = NULL,
    errorMin = NULL,
    errorMax = NULL,
    color = NULL,
    size = NULL,
    shape = NULL,
    linetype = NULL,
    data = NULL,

    initialize = function(x, y,
                          ###
                          error = NULL,
                          errorMin = NULL,
                          errorMax = NULL,
                          ###
                          color    = NULL, #string or vector of strings that are dataframe colnames to group by color
                          size     = NULL, #string or vector of strings that are dataframe colnames to group by size
                          shape    = NULL, #string or vector of strings that are dataframe colnames to group by shape
                          linetype = NULL, #string or vector of strings that are dataframe colnames to group by linetype
                          ###
                          colorGroupingDataFrame = NULL,
                          sizeGroupingDataFrame = NULL,
                          shapeGroupingDataFrame = NULL,
                          linetypeGroupingDataFrame = NULL,
                          ###
                          colorLegendTitle    = NULL,
                          sizeLegendTitle     = NULL,
                          shapeLegendTitle    = NULL,
                          linetypeLegendTitle = NULL,
                          ###
                          data     = NULL,
                          metaData = NULL){

      super$initialize(x=x,
                       y=y,
                       error = error,
                       errorMin = errorMin,
                       errorMax = errorMax,
                       color=color,
                       size=size,
                       shape=shape,
                       linetype=linetype,
                       colorGroupingDataFrame=colorGroupingDataFrame,
                       sizeGroupingDataFrame=sizeGroupingDataFrame,
                       shapeGroupingDataFrame=shapeGroupingDataFrame,
                       linetypeGroupingDataFrame=linetypeGroupingDataFrame,
                       colorLegendTitle=colorLegendTitle,
                       sizeLegendTitle=sizeLegendTitle,
                       shapeLegendTitle=shapeLegendTitle,
                       linetypeLegendTitle=linetypeLegendTitle,
                       data=data,
                       metaData=metaData)

    }
  )
)

