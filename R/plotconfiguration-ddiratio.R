#' @title DDIRatioPlotConfiguration
#' @docType class
#' @description  Plot Configuration for DDIRatio
#' @export
DDIRatioPlotConfiguration <- R6::R6Class(
  "DDIRatioPlotConfiguration",
  inherit = PlotConfiguration,
  public = list(
    ddiRatioProperties = NULL,
    
    initialize = function(ddiRatioProperties = tlfEnv$currentTheme$ddiRatio,
                          title = "DDI Ratio Plot",
                          subtitle = paste("Date:", format(Sys.Date(), "%y-%m-%d")),
                          ...) {
      super$initialize(
        title = title,
        subtitle = subtitle,
        ...
      )
      
      self$ddiRatioProperties <- ddiRatioProperties
    },
    
    addDDIRatioLines = function(plotObject, dataMapping) {
      # geom_abline don't work properly when plot scale is log
      ddiRatioLines <- dataMapping$getDDIRatioLines()
      # Remove x from names
      ddiRatioLabels <- utils::tail(names(ddiRatioLines),-1)
      for (RatioIndex in seq(1, length(ddiRatioLabels))) {
        plotObject <- plotObject +
          ggplot2::geom_line(data=ddiRatioLines, 
                             mapping=aes_string(x="x", y=ddiRatioLabels[RatioIndex]),
            linetype = self$ddiRatioProperties$lines$linetype[RatioIndex],
            color = self$ddiRatioProperties$lines$color[RatioIndex],
            size = self$ddiRatioProperties$lines$size[RatioIndex]
          )
      }
      return(plotObject)
    },
    
    addGuestLines = function(plotObject, dataMapping) {
      guestLines <- dataMapping$getGuestLines()
      plotObject <- plotObject +
          ggplot2::geom_line(data=guestLines, 
                             mapping=aes_string(x="x", y="ymax"),
                             linetype = self$ddiRatioProperties$guest$linetype,
                             color = self$ddiRatioProperties$guest$color,
                             size = self$ddiRatioProperties$guest$size) + 
          ggplot2::geom_line(data=guestLines, 
                             mapping=aes_string(x="x", y="ymin"),
                             linetype = self$ddiRatioProperties$guest$linetype,
                             color = self$ddiRatioProperties$guest$color,
                             size = self$ddiRatioProperties$guest$size)
      return(plotObject)
    },
    
    addDDIRatios = function(plotObject, data, metaData, dataMapping) {
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
