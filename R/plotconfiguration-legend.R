#' @title LegendConfiguration
#' @docType class
#' @description  Class for Legend Configuration
#' @field position Legend position as defined in LegendPositions enum
#' @field titles R6 class defining titles of legend
#' @field captions R6 class defining captions of legend
#' @field values R6 class defining values of aesthetic properties (e.g. circle, square for shape)
#' @section Methods:
#' \describe{
#' \item{new(position = NULL, titles = NULL, captions = NULL, values = NULL, dataMapping = NULL,
#' data = NULL, metaData = NULL, theme = tlfEnv$currentTheme)}{Initialize LegendConfiguration.}
#' \item{setPlotLegend(plotObject)}{Apply properties to plot legend.}
#' }
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    position = NULL,
    titles = NULL,
    captions = NULL,
    values = NULL,

    initialize = function(position = NULL,
                              titles = NULL,
                              captions = NULL,
                              values = NULL,
                              dataMapping = NULL,
                              data = NULL,
                              metaData = NULL,
                              theme = tlfEnv$currentTheme) {
      ifnotnull(
        position,
        self$position <- position,
        self$position <- LegendPositions$outsideRight
      )

      if (!is.null(dataMapping) && !is.null(data)) {
        if ("XYGDataMapping" %in% class(dataMapping)) {
          self$titles <- LegendTitles$new(groupings = dataMapping$groupMapping)
          self$captions <- LegendCaptions$new(
            groupings = dataMapping$groupMapping,
            data = data,
            metaData = metaData
          )
        }
      } else {
        self$titles <- titles %||% LegendTitles$new()
        self$captions <- captions %||% LegendCaptions$new()
      }
      self$values <- captions %||% LegendValues$new()
    },

    setPlotLegend = function(plotObject) {
      plotObject <- setLegend(plotObject,
        legendPosition = self$position,
        legendTitles = self$titles,
        legendCaptions = self$captions,
        legendValues = self$values
      )

      plotObject <- setLegendPosition(plotObject, legendPosition = self$position)

      return(plotObject)
    }
  )
)
