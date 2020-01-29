#' @title GroupMapping
#' @description  R6 class for mapping \code{Grouping} variables to \code{data}
#' @export
GroupMapping <- R6::R6Class(
  "GroupMapping",
  public = list(
    #' @field color R6 class \code{Grouping} object
    color = NULL,
    #' @field fill R6 class \code{Grouping} object
    fill = NULL,
    #' @field linetype R6 class \code{Grouping} object
    linetype = NULL,
    #' @field shape R6 class \code{Grouping} object
    shape = NULL,
    #' @field size R6 class \code{Grouping} object
    size = NULL,

    #' @description Create a new \code{GroupMapping} object
    #' @param color R6 class \code{Grouping} object or its input
    #' @param fill R6 class \code{Grouping} object or its input
    #' @param linetype R6 class \code{Grouping} object or its input
    #' @param shape R6 class \code{Grouping} object or its input
    #' @param size R6 class \code{Grouping} object or its input
    #' @return A new \code{GroupMapping} object
    initialize = function(color = NULL,
                              fill = NULL,
                              linetype = NULL,
                              shape = NULL,
                              size = NULL) {
      # If mappings are not of type Grouping, they are assumed as Grouping inputs
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
