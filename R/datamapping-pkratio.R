#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field groupMapping R6 class mapping groups to aesthetic properties
#' @field pkRatioLines Values of the horizontal lines plotted in PK ratio plots
#' @section Methods:
#' \describe{
#' \item{new(pkRatioLines, x, y, groupMapping = NULL, color = NULL, fill = NULL, linetype = NULL, shape = NULL, size = NULL)}{
#' Initialize PKRatioDataMapping Either input groupMapping or input color, fill, linetype, shape and/or size.}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' }
#' @export
PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYGDataMapping,
  public = list(
    pkRatioLines = NULL,

    initialize = function(pkRatioLines = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
                              x = "Age",
                              y = "Ratio", ...) {
      super$initialize(x = x, y = y, ...)
      self$pkRatioLines <- pkRatioLines
    }
  )
)
