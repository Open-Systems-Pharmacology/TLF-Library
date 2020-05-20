#' @title TimeProfileDataMapping
#' @description  R6 class defining the configuration of a \code{ggplot} object for time profile plot
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = XYGDataMapping,

  public = list(
    #' @field lloq numeric value of lower limit of quantification
    lloq = NULL,
    #' @field isRangeTimeProfile logical to set aggregation of data
    isRangeTimeProfile = NULL,

    #' @description Create a new \code{TimeProfileDataMapping} object
    #' @param lloq numeric value of lower limit of quantification
    #' @param isRangeTimeProfile Name of x variable to map
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{TimeProfileDataMapping} object
    initialize = function(lloq = NULL,
                              isRangeTimeProfile = FALSE,
                              ...) {
      validateIsLogical(isRangeTimeProfile)
      super$initialize(...)
      self$lloq <- lloq
      self$isRangeTimeProfile <- isRangeTimeProfile
    }
  )
)
