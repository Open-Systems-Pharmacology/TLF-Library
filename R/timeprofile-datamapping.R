#' @title TimeProfileDataMapping
#' @docType class
#' @description  Data Mapping for Time Profile
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  public = list(
    simulationSets = NULL,
    observationSets = NULL,
    simulationMapping = NULL,
    observationMapping = NULL,
    LLOQ = NULL,

    # Example of how to do some other stuff
    initialize = function(simulationSets = 1,
                              observationSets = NULL,
                              simulationMapping = NULL,
                              observationMapping = NULL,
                              LLOQ = NULL) {
      self$simulationSets <- simulationSets
      self$simulationMapping <- simulationMapping %||% XYDataMapping$new(x = "Time", y = "Value")
      self$observationSets <- observationSets
      self$observationMapping <- ifnotnull(
        observationSets,
        observationMapping %||% XYDataMapping$new(x = "Time", y = "Value"),
        observationMapping
      )
    }
  )
)
