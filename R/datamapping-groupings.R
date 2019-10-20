#' @title Groupings
#' @docType class
#' @description  Abstract class for Groupings
#' @export
Groupings <- R6::R6Class(
  "Groupings",
  public = list(
    color = NULL,
    fill = NULL,
    linetype = NULL,
    shape = NULL,

    initialize = function(color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL) {

      if (!isOfType(color, "GroupMapping")) {
        color <- GroupMapping$new(group = color)
      }
      if (!isOfType(fill, "GroupMapping")) {
        fill <- GroupMapping$new(group = fill)
      }
      if (!isOfType(linetype, "GroupMapping")) {
        linetype <- GroupMapping$new(group = linetype)
      }
      if (!isOfType(shape, "GroupMapping")) {
        shape <- GroupMapping$new(group = shape)
      }

      self$color <- color
      self$fill <- fill
      self$linetype <- linetype
      self$shape <- shape
    }
  )
)
