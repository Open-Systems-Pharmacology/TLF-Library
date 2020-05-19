#' @title LegendConfiguration
#' @description R6 class defining the legend configuration of a \code{ggplot} object
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    #' @field position character position of the legend
    position = NULL,
    #' @field title character name of the legend
    title = NULL,
    #' @field caption data.frame with
    caption = NULL,

    #' @description Create a new \code{LegendConfiguration} object
    #' @param position character position of the legend.
    #' Use enum `LegendPositions` to assess available legend positions.
    #' @param title character title of the legend caption.
    #' Default `NULL` does not provide any legend title.
    #' @param caption data.frame containing the legend caption properties
    #' @return A new \code{LegendConfiguration} object
    initialize = function(position = tlfEnv$defaultLegendPosition,
                              title = NULL,
                              caption = NULL) {
      validateIsIncluded(position, LegendPositions)

      self$position <- position
      self$title <- title
      self$caption <- caption
    },

    #' @description Print legend properties
    #' @return Legend properties
    print = function() {
      legendProperties <- list(
        title = self$title,
        position = self$position,
        caption = self$caption
      )
      return(legendProperties)
    }
  )
)
