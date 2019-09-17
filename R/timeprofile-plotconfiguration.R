#' @title TimeProfilePlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
TimeProfilePlotConfiguration <- R6::R6Class(
  "TimeProfilePlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    LLOQLineProperties = NULL,

    initialize = function(title = "Time Profile Plot",
                          LLOQLineProperties = NULL,
                          dataMapping = NULL,
                          metaData = NULL,
                          ...) {
      timeProfileDataMapping <- NULL
      timeProfileMetaData <- NULL
      if (!is.null(dataMapping)) {
        timeProfileDataMapping <- dataMapping$simulationMapping
        timeProfileMetaData <- metaData[[dataMapping$simulationSets]]
      }

      super$initialize(
        title = title,
        metaData = timeProfileMetaData,
        dataMapping = timeProfileDataMapping,
        ...
      )
    },

    addLLOQLine = function(plotHandle) {
      plotHandle <- plotHandle +
        ifnotnull(self$LLOQLineProperties, ggplot2::geom_hline(
          yintercept = self$LLOQLineProperties$value,
          linetype = self$LLOQLineProperties$linetype,
          color = self$RatioLineProperties$color,
          size = self$RatioLineProperties$size
        ))
      return(plotHandle)
    }
  )
)
