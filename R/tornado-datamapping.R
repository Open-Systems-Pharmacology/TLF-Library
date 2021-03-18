#' @title TornadoDataMapping
#' @description  R6 class for mapping \code{values}, \code{labels} to \code{data}
#' @export
TornadoDataMapping <- R6::R6Class(
  "TornadoDataMapping",
  inherit = XYGDataMapping,
  public = list(
    #' @field lines numeric vector of limits to plot
    lines = NULL,
    #' @field sorted logical indicating if values should be sorted
    sorted = NULL,

    #' @description Create a new \code{TornadoDataMapping} object
    #' @param lines numeric vector of limits to plot
    #' @param sorted logical indicating if values should be sorted
    #' @param x Variable including the values of tornado plot
    #' @param y Variable including the labels of tornado plot
    #' @param ... parameters inherited from \code{XYGDataMapping}
    #' @return A new \code{TornadoDataMapping} object
    initialize = function(lines = DefaultDataMappingValues$tornado,
                              sorted = NULL,
                              x = NULL,
                              y = NULL,
                              ...) {
      validateIsNumeric(lines, nullAllowed = TRUE)
      validateIsLogical(sorted, nullAllowed = TRUE)
      super$initialize(x = x, y = y, ...)

      # The next lines aim at linking some variables in case they are not
      # explicitely defined by users, this leads to nice plot legends in the end

      # Link color to labels (y) if color is not explicitely mapped
      self$groupMapping$color$label <- self$groupMapping$color$label %||% y
      self$groupMapping$color$group <- self$groupMapping$color$group %||% y
      # Link fill/shape to color if they are not explicitely mapped
      self$groupMapping$fill <- ifnotnull(
        self$groupMapping$fill$label,
        self$groupMapping$fill,
        self$groupMapping$color
      )
      self$groupMapping$shape <- ifnotnull(
        self$groupMapping$shape$label,
        self$groupMapping$shape,
        self$groupMapping$color
      )

      self$lines <- lines
      self$sorted <- sorted %||% TRUE
    }
  )
)
