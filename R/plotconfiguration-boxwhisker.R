#' @title BoxWhiskerPlotConfiguration
#' @docType class
#' @description  Plot Configuration for Box Whisker Plots
#' @export
BoxWhiskerPlotConfiguration <- R6::R6Class(
  "BoxWhiskerPlotConfiguration",
  inherit = PlotConfiguration,
  
  public = list(
    
    initialize = function(title = "Time Profile Plot",
                          subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                          ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )
    },
    
    addBoxWhisker = function(plotObject, data, metaData, dataMapping) {
      
      mapData <- dataMapping$checkMapData(mapData, metaData)
      
      # Get the box plot quantiles from getBoxWhiskerMeasure
      mapQuantiles <- dataMapping$getQUantiles(data)
      
      # Convert the mapping into characters usable by aes_string
      mapLabels <- getAesStringMapping(dataMapping)
      
      plotObject <- plotObject + ggplot2::geom_boxplot(data = mapQuantiles,
                             mapping = aes_string(x=mapLabels$x,
                                                  ymin = mapLabels$yMin,
                                                  lower = mapLabels$lower,
                                                  middle = mapLabels$middle,
                                                  upper = mapLabels$upper,
                                                  ymax = mapLabels$yMax,
                                                  fill = mapLabels$fill,
                                                  color = mapLabels$color,
                                                  linetype = mapLabels$linetype,
                                                  size = mapLabels$size),
                             show.legend = TRUE,
                             stat ="identity")
      
      plotObject <- plotObject +
            ifEqual("defaultAes", mapLabels$fill, guides(color = "none")) +
            ifEqual("defaultAes", mapLabels$color, guides(color = "none")) +
            ifEqual("defaultAes", mapLabels$linetype, guides(linetype = "none")) +
            ifEqual("defaultAes", mapLabels$size, guides(size = "none"))
        
      return(plotObject)
    },
    
    addOutliers = function(plotObject, data, metaData, dataMapping) {}
  )
)
