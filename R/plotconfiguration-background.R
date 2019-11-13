#' @title BackgroundConfiguration
#' @docType class
#' @description  Generic Background Configuration
#' @export
BackgroundConfiguration <- R6::R6Class(
  "BackgroundConfiguration",
  public = list(
    outerBackground = NULL,
    innerBackground = NULL,
    grid = NULL,
    watermark = NULL,
    
    initialize = function(outerBackground = NULL,
                          innerBackground = NULL,
                          grid = NULL,
                          watermark = NULL,
                          watermarkFont = NULL,
                          theme = tlfEnv$currentTheme){
      
      self$outerBackground <- outerBackground %||% BackgroundElementConfiguration$new(theme = theme$background$outer)
      self$innerBackground <- innerBackground %||% BackgroundElementConfiguration$new(theme = theme$background$inner)
      self$grid <- grid %||% BackgroundElementConfiguration$new(theme = theme$background$grid)
      
      self$watermark <- asLabel(watermark %||% theme$background$watermark %||% "")
      self$watermark$font <- watermarkFont %||% theme$watermarkFont
    },
    
    setBackground = function(plotObject){
      plotObject <- plotObject + theme(plot.background = element_rect(fill = self$outerBackground$fill),
                                       panel.background = element_rect(fill = self$innerBackground$fill, 
                                                                       color = self$innerBackground$color,
                                                                       size = self$innerBackground$size,
                                                                       linetype = self$innerBackground$linetype,
                                       ),
                                       panel.grid = element_line(color = self$grid$color,
                                                                 size = self$grid$size,
                                                             linetype = self$grid$linetype))
      plotObject <- setWatermark(plotObject = plotObject, label = self$watermark)
      return(plotObject)
    }
  )
  )


BackgroundElementConfiguration <- R6::R6Class(
  "BackgroundElementConfiguration",
  public = list(
    fill = NULL,
    color = NULL,
    size = NULL,
    linetype = NULL,
    
    initialize = function(fill = NULL,
                          color = NULL,
                          size = NULL,
                          linetype = NULL,
                          theme = NULL){
      
      self$fill <- fill %||% theme$fill
      self$color <- color %||% theme$color
      self$size <- size %||% theme$size
      self$linetype <- linetype %||% theme$linetype
    }
  )
)
    