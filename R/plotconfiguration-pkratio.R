#' @title PKRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for PKRatio
#' @export
PKRatioPlotConfiguration <- R6::R6Class(
  "PKRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    pkRatioLinesProperties = NULL,
    colorGrouping = NULL,
    shapeGrouping = NULL,

    initialize = function(pkRatioLinesProperties = tlfEnv$currentTheme$pkRatioLinesProperties,
                              colorGrouping = NULL,
                              shapeGrouping = NULL,
                              title = "PK Ratio Plot",
                              subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = tlfEnv$currentTheme$watermarkText,
                              data = NULL,
                              metaData = NULL,
                              dataMapping = NULL,
                              ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        xlabel = xlabel,
        ylabel = ylabel,
        watermark = watermark,
        data = data,
        metaData = metaData,
        dataMapping = dataMapping
      )

      self$pkRatioLinesProperties <- pkRatioLinesProperties

      if (!is.null(dataMapping$groupings)) {
        self$colorGrouping <- dataMapping$groupings[["color"]]$groupingName[[1]]
        self$shapeGrouping <- dataMapping$groupings[["shape"]]$groupingName[[1]]
      }

      # Overwrite grouping if config is different from Mapping
      self$colorGrouping <- colorGrouping %||% self$colorGrouping
      self$shapeGrouping <- shapeGrouping %||% self$shapeGrouping


      if (is.null(self$shapeGrouping)) {
        # self$legend$captions$color <- NULL
        self$colorGrouping <- "color"
      }
      if (is.null(self$shapeGrouping)) {
        # self$legend$captions$shape <- NULL
        self$shapeGrouping <- "shape"
      }
    },

    addPKRatioLines = function(pkRatioLines, plotObject) {
      for (RatioIndex in seq(1, length(pkRatioLines))) {
        plotObject <- plotObject +
          ggplot2::geom_hline(
            yintercept = pkRatioLines[RatioIndex],
            linetype = self$pkRatioLinesProperties$linetype[RatioIndex],
            color = self$pkRatioLinesProperties$color[RatioIndex],
            size = self$pkRatioLinesProperties$size[RatioIndex]
          )
      }
      return(plotObject)
    },

    addPKRatios = function(plotObject, data, metaData, dataMapping) {
      mapData <- dataMapping$getMapData(data, metaData)

      if (isTRUE(self$colorGrouping == self$shapeGrouping)) {
        plotObject <- plotObject + geom_point(
          mapping = aes(
            x = x, y = y,
            color = color,
            shape = color
          ),
          data = mapData,
          size = 1, # To be updated with current Theme
          show.legend = TRUE
        )
      } else {
        plotObject <- plotObject + geom_point(
          mapping = aes(
            x = x, y = y,
            color = color,
            shape = shape
          ),
          data = mapData,
          size = 1, # To be updated with current Theme
          show.legend = TRUE
        )
      }
      return(plotObject)
    }
  )
)
