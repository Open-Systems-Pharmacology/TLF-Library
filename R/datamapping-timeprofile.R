#' @title TimeProfileDataMapping
#' @docType class
#' @description  Data Mapping for Time Profile
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = XYDataMapping,
  public = list(
    initialize = function(...) {
      super$initialize(...)
    }
  )
)
