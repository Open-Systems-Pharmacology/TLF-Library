#' @title LegendConfiguration
#' @docType class
#' @description  Generic Legend Configuration
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    position = NULL,
    # next variables are R6 classes with aesthetic propoerties as names
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
          self$titles <- LegendTitles$new(groupings = dataMapping$groupings)
          self$captions <- LegendCaptions$new(
            groupings = dataMapping$groupings,
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

      return(plotObject)
    }
  )
)
