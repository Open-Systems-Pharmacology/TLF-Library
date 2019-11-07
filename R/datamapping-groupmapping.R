#' @title GroupMapping
#' @docType class
#' @description  Abstract class for GroupMapping
#' @export
GroupMapping <- R6::R6Class(
  "GroupMapping",
  public = list(
    color = NULL,
    fill = NULL,
    linetype = NULL,
    shape = NULL,
    size = NULL,

    initialize = function(color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL) {
      if (!isOfType(color, "Grouping")) {
        color <- Grouping$new(group = color)
      }
      if (!isOfType(fill, "Grouping")) {
        fill <- Grouping$new(group = fill)
      }
      if (!isOfType(linetype, "Grouping")) {
        linetype <- Grouping$new(group = linetype)
      }
      if (!isOfType(shape, "Grouping")) {
        shape <- Grouping$new(group = shape)
      }
      if (!isOfType(size, "Grouping")) {
        size <- Grouping$new(group = size)
      }

      self$color <- color
      self$fill <- fill
      self$linetype <- linetype
      self$shape <- shape
      self$size <- size
    }
  )
)
