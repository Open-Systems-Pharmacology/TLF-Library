#' @title LegendConfiguration
#' @description R6 class defining the legend configuration of a \code{ggplot} object
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    #' @field position character position of the legend
    position = NULL,
    #' @field titles R6 class \code{LegendTitles} object
    titles = NULL,
    #' @field captions R6 class \code{LegendCaptions} object
    captions = NULL,
    #' @field values R6 class \code{LegendValues} object
    values = NULL,

    #' @description Create a new \code{LegendConfiguration} object
    #' @param position character position of the legend.
    #' Use enum `LegendPositions` to assess available legend positions.
    #' @param titles R6 class \code{LegendTitles} object
    #' @param captions R6 class \code{LegendCaptions} object
    #' @param values R6 class \code{LegendValues} object
    #' @param data data.frame used by \code{smartMapping}
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class or subclass \code{XYDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @return A new \code{LegendConfiguration} object
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
      }

      self$titles <- titles %||% self$titles %||% LegendTitles$new()
      self$captions <- captions %||% self$captions %||% LegendCaptions$new()
      self$values <- values %||% LegendValues$new()
    },

    #' @description Set plot legend of a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @return A \code{ggplot} object
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
