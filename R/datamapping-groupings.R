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
      if (!"GroupMapping" %in% class(color)) {
        color <- GroupMapping$new(group = color)
      }
      if (!"GroupMapping" %in% class(fill)) {
        fill <- GroupMapping$new(group = fill)
      }
      if (!"GroupMapping" %in% class(linetype)) {
        linetype <- GroupMapping$new(group = linetype)
      }
      if (!"GroupMapping" %in% class(shape)) {
        shape <- GroupMapping$new(group = shape)
      }

      self$color <- color
      self$fill <- fill
      self$linetype <- linetype
      self$shape <- shape
    }
  )
)
