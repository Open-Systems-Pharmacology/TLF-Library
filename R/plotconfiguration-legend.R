#' @title LegendConfiguration
#' @docType class
#' @description  Generic Legend Configuration
#' @export
LegendConfiguration <- R6::R6Class(
  "LegendConfiguration",
  public = list(
    position = NULL,
    aesProperties = NULL,
    titles = NULL,
    captions = NULL,
    values = NULL,

    initialize = function(position = NULL,
                              aesProperties = NULL,
                              titles = NULL,
                              captions = NULL,
                              values = tlfEnv$currentTheme$aesProperties,
                              dataMapping = NULL,
                              data = NULL,
                              metaData = NULL,
                              theme = tlfEnv$currentTheme) {
      ifnotnull(
        position,
        self$position <- position,
        self$position <- legendPositions$outsideRight
      )

      mapTitles <- list()
      mapCaptions <- list()
      if (!is.null(dataMapping$groupings)) {
        mapData <- dataMapping$getMapData(data, metaData)
        for (group in dataMapping$groupingNames) {
          mapTitles[[group]] <- dataMapping$groupings[[group]]$groupingLegendTitle %||% paste(dataMapping$groupings[[group]]$groupingName[[1]], collapse = "-")
          mapCaptions[[group]] <- mapData[, group]
        }
      }
      self$titles <- titles %||% mapTitles
      self$captions <- captions %||% mapCaptions

      self$aesProperties <- aesProperties %||% dataMapping$groupingNames

      self$values <- values
    },

    setPlotLegend = function(plotObject) {
      plotObject <- setLegendPosition(plotObject, legendPosition = self$position)

      for (aesProperty in self$aesProperties) {
        plotObject <- plotObject +
          ifnotnull(
            self$captions[[aesProperty]],

            scale_discrete_manual(
              aesthetics = aesProperty,
              name = self$titles[[aesProperty]],
              values = self$values[[aesProperty]],
              labels = self$captions[[aesProperty]]
            ),

            scale_discrete_manual(
              aesthetics = aesProperty,
              name = self$titles[[aesProperty]],
              values = self$values[[aesProperty]],
              labels = NA,
              guide = "none"
            )
          )
      }
      return(plotObject)
    }
  )
)