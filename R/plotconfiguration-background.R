#' @title BackgroundConfiguration
#' @description R6 class defining the configuration of background
#' @export
BackgroundConfiguration <- R6::R6Class(
  "BackgroundConfiguration",
  public = list(
    #' @field outerBackground R6 class \code{BackgroundElementConfiguration} object
    outerBackground = NULL,
    #' @field innerBackground R6 class \code{BackgroundElementConfiguration} object
    innerBackground = NULL,
    #' @field grid R6 class \code{BackgroundElementConfiguration} object
    grid = NULL,
    #' @field watermark R6 class \code{Label} object defining watermark background
    watermark = NULL,

    #' @description Create a new \code{BackgroundConfiguration} object
    #' @param outerBackground R6 class \code{BackgroundElementConfiguration} object
    #' @param innerBackground R6 class \code{BackgroundElementConfiguration} object
    #' @param grid R6 class \code{BackgroundElementConfiguration} object
    #' @param watermark R6 class \code{Label} object defining watermark background
    #' @param watermarkFont R6 class \code{Font} object defining watermark font
    #' @param theme R6 class \code{Theme} object
    #' @return A new \code{BackgroundConfiguration} object
    initialize = function(outerBackground = NULL,
                              innerBackground = NULL,
                              grid = NULL,
                              watermark = NULL,
                              watermarkFont = NULL,
                              theme = tlfEnv$currentTheme) {
      self$outerBackground <- outerBackground %||% BackgroundElementConfiguration$new(theme = theme$background$outer)
      self$innerBackground <- innerBackground %||% BackgroundElementConfiguration$new(theme = theme$background$inner)
      self$grid <- grid %||% BackgroundElementConfiguration$new(theme = theme$background$grid)

      self$watermark <- asLabel(watermark %||% theme$background$watermark %||% "")
      self$watermark$font <- watermarkFont %||% theme$watermarkFont
    },

    #' @description Set background properties of a \code{ggplot} object
    #' @param plotObject a \code{ggplot} object
    #' @return A \code{ggplot} object
    setBackground = function(plotObject) {
      plotObject <- plotObject + theme(
        plot.background = element_rect(fill = self$outerBackground$fill),
        panel.background = element_rect(
          fill = self$innerBackground$fill,
          color = self$innerBackground$color,
          size = self$innerBackground$size,
          linetype = self$innerBackground$linetype,
        ),
        panel.grid = element_line(
          color = self$grid$color,
          size = self$grid$size,
          linetype = self$grid$linetype
        )
      )
      plotObject <- addWatermark(plotObject = plotObject, label = self$watermark)
      return(plotObject)
    }
  )
)

#' @title BackgroundElementConfiguration
#' @description  R6 class defining the configuration of background elements
#' @export
BackgroundElementConfiguration <- R6::R6Class(
  "BackgroundElementConfiguration",
  public = list(
    #' @field fill character color filling of the background element
    fill = NULL,
    #' @field color character color of the frame of the background element
    color = NULL,
    #' @field size character size of the frame of the background element
    size = NULL,
    #' @field linetype character linetype of the frame of the background element
    linetype = NULL,

    #' @description Create a new \code{BackgroundElementConfiguration} object
    #' @param fill character color filling of the background element
    #' @param color character color of the frame of the background element
    #' @param size character size of the frame of the background element
    #' @param linetype R6 class \code{Grouping} object or its input
    #' @param theme R6 class \code{Theme} object
    #' @return A new \code{BackgroundElementConfiguration} object
    initialize = function(fill = NULL,
                              color = NULL,
                              size = NULL,
                              linetype = NULL,
                              theme = NULL) {
      self$fill <- fill %||% theme$fill
      self$color <- color %||% theme$color
      self$size <- size %||% theme$size
      self$linetype <- linetype %||% theme$linetype
    }
  )
)
