#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    RatioLinesProperties = NULL,

    initialize = function(title = "PK Ratio Plot",
                          RatioLinesProperties = data.frame(
                            value = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
                            linetype = c("solid", "dashed", "dashed", "dashed", "dashed"),
                            color = c("black", "blue", "blue", "red", "red"),
                            size = c(2, 1, 1, 1, 1)
                          ),
                          ...) {
      super$initialize(
        title = title,
        ...
      )

      self$RatioLinesProperties <- RatioLinesProperties
      # colors and linetype are assumed as levels by default
      # they need to be reassessed as character
      self$RatioLinesProperties$color <- as.character(self$RatioLinesProperties$color)
      self$RatioLinesProperties$linetype <- as.character(self$RatioLinesProperties$linetype)
    },

    addRatioLines = function(plotHandle) {
      for (RatioIndex in seq(1, length(self$RatioLinesProperties$value))) {
        plotHandle <- plotHandle +
          ggplot2::geom_hline(
            yintercept = self$RatioLinesProperties$value[RatioIndex],
            linetype = self$RatioLinesProperties$linetype[RatioIndex],
            color = self$RatioLinesProperties$color[RatioIndex],
            size = self$RatioLinesProperties$size[RatioIndex]
          )
      }
      return(plotHandle)
    }
  )
)
