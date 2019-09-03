#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    RatioLinesProperties = data.frame(
      ratio = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
      linetype = c("solid", "dashed", "dashed", "dashed", "dashed"),
      color = c("black", "blue", "blue", "red", "red"),
      size = c(2, 1, 1, 1, 1)
    ),
    initialize = function(title = asLabel("PK Ratio Plot"),
                          xlabel = asLabel("Age [yrs]"),
                          ylabel = asLabel("Simulated/Observed Ratio"),
                          RatioLinesProperties = data.frame(
                            ratio = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
                            linetype = c("solid", "dashed", "dashed", "dashed", "dashed"),
                            color = c("black", "blue", "blue", "red", "red"),
                            size = c(2, 1, 1, 1, 1)
                          ),
                          ...) {
      super$initialize(
        title = title,
        xlabel = xlabel,
        ylabel = ylabel,
        ...
      )

      self$RatioLinesProperties <- RatioLinesProperties
    },

    addRatioLines = function(plotHandle) {
      for (RatioIndex in seq(1, length(self$RatioLinesProperties$ratio))) {
        plotHandle <- plotHandle +
          geom_hline(
            yintercept = self$RatioLinesProperties$ratio[RatioIndex],
            linetype = as.character(self$RatioLinesProperties$linetype[RatioIndex]),
            color = as.character(self$RatioLinesProperties$color[RatioIndex]),
            size = self$RatioLinesProperties$size[RatioIndex]
          )
      }
      return(plotHandle)
    }
  )
)