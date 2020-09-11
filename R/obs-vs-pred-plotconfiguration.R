#' @title ObsVsPredPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for Obs vs Pred plots
#' @export
ObsVsPredPlotConfiguration <- R6::R6Class(
  "ObsVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field obsVsPredCaption list of properties for obs vs pred plot specific features
    obsVsPredCaption = NULL,

    #' @description Create a new \code{ObsVsPredPlotConfiguration} object
    #' @param obsVsPredCaption list of properties for DDI ratio plot specific features
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{obsVsPredProperties} object
    initialize = function(obsVsPredCaption = getDefaultCaptionFor("obsVsPred"),
                              ...) {
      validateIsOfType(obsVsPredCaption, "data.frame")
      validateIsIncluded(names(obsVsPredCaption), CaptionProperties)
      super$initialize(...)

      self$obsVsPredCaption <- obsVsPredCaption
    },

    #' @description Add smoother layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{ObsVsPredDataMapping}
    #' @return A \code{ggplot} object with smoother
    addSmoother = function(plotObject, data, metaData, dataMapping) {
      # Check if mapping is included in the data
      # Add the group mapping and aesthtics variables in the data.frame
      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + geom_smooth(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          linetype = mapLabels$linetype,
          size = mapLabels$size
        ),
        method = dataMapping$smoother,
        se = FALSE,
        show.legend = TRUE
      )
      return(plotObject)
    },

    #' @description Add obs vs pred as scatter layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class \code{ObsVsPredDataMapping}
    #' @return A \code{ggplot} object
    addObsVsPred = function(plotObject, data, metaData, dataMapping) {

      # Check if mapping is included in the data
      # Add the group mapping and aesthtics variables in the data.frame
      mapData <- dataMapping$checkMapData(data, metaData)

      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)

      plotObject <- plotObject + geom_point(
        data = mapData,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          shape = mapLabels$shape,
          size = mapLabels$size
        ),
        show.legend = TRUE
      )

      # If no mapping defined, remove dummy aesthetic label from the legend
      plotObject <- plotObject +
        ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
        ifEqual("defaultAes", mapLabels$shape, guides(shape = "none")) +
        ifEqual("defaultAes", mapLabels$size, guides(size = "none"))

      return(plotObject)
    }
  )
)
