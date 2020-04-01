#' @title ObsVsPredPlotConfiguration
#' @description R6 class defining the configuration of a \code{ggplot} object for Obs vs Pred plots
#' @export
ObsVsPredPlotConfiguration <- R6::R6Class(
  "ObsVsPredPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    #' @field obsVsPredProperties list of properties for obs vs pred plot specific features
    obsVsPredProperties = NULL,
    
    #' @description Create a new \code{ObsVsPredPlotConfiguration} object
    #' @param obsVsPredProperties list of properties for DDI ratio plot specific features
    #' @param title R6 class \code{Label} object
    #' @param subtitle R6 class \code{Label} object
    #' @param xlabel R6 class \code{Label} object
    #' @param ylabel R6 class \code{Label} object
    #' @param legend R6 class \code{LegendConfiguration} object defining legend properties
    #' @param legendTitles List of legend titles
    #' @param xAxis R6 class \code{XAxisConfiguration} object defining X-axis properties
    #' @param xScale character defining X-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param xLimits numeric vector of X-axis limits
    #' @param yAxis R6 class \code{YAxisConfiguration} object defining X-axis properties
    #' @param yScale character defining Y-axis scale. Use enum `Scaling` to access predefined scales.
    #' @param yLimits numeric vector of Y-axis limits
    #' @param background R6 class \code{BackgroundConfiguration} defining background properties
    #' @param watermark R6 class \code{Label} object defining watermark background
    #' @param saveConfiguration R6 class \code{SaveConfiguration} defining saving properties
    #' @param filename character defining the name of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param data data.frame used by \code{smartMapping}
    #' @param metaData list of information on \code{data}
    #' @param dataMapping R6 class or subclass \code{XYGDataMapping}
    #' @param theme R6 class \code{Theme}
    #' @param ... parameters inherited from \code{PlotConfiguration}
    #' @return A new \code{obsVsPredProperties} object
    initialize = function(obsVsPredProperties = tlfEnv$currentTheme$obsVsPred,
                          title = "Obs vs Pred Plot",
                          subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                          ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )
      
      self$obsVsPredProperties <- obsVsPredProperties
    },
    
    #' @description Add identity as line layer to a \code{ggplot} object
    #' @param plotObject \code{ggplot} object
    #' @param data data.frame
    #' @param dataMapping R6 class \code{ObsVsPredDataMapping}
    #' @return A \code{ggplot} object
    addObsVsPredLines = function(plotObject, data, dataMapping) {
      # geom_abline don't work properly when plot scale is log
      obsVsPredLines <- dataMapping$getObsVsPredLines(data)
      
      plotObject <- addLine(data = obsVsPredLines,
                            plotObject = plotObject)
      return(plotObject)
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
