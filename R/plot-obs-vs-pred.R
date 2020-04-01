#' @title plotObsVsPred
#' @param data data.frame containing the data to be used for the plot
#' @param metaData list of lists
#' containing complementary information to data (e.g. their unit and dimension).
#' This parameter is optional.
#' @param dataMapping
#' \code{ObsVsPredDataMapping} class or subclass mapping x and y variables to \code{data} variable names.
#' \code{dataMapping} provides also the values of the identity and fold errors lines.
#' This parameter is optional: the \code{tlf} library provides a smart mapping if only \code{data} is provided
#' and default values of the identity and fold errors lines.
#' @param plotConfiguration
#' \code{ObsVsPredConfiguration} class or subclass defining labels, grid, background and watermark
#' This parameter is optional: the \code{tlf} library provides a default configuration according to the current theme
#' @param plotObject \code{ggplot} graphical object to which the Observations vs Predictions plot layer is added
#' This parameter is optional: the \code{tlf} library will initialize an empty plot if the parameter is NULL or not provided
#' @description
#' Add Observations vs Predictions plot layers to a \code{ggplot} graphical object.
#' Identity line is plotted as a diagonal line (TO DO: integrate fold errors lines).
#' Observations vs Predictions are plotted as a scatter plot.
#' @return A \code{ggplot} graphical object
#' @export
plotObsVsPred <- function(data,
                        metaData = NULL,
                        dataMapping = NULL,
                        plotConfiguration = NULL,
                        plotObject = NULL) {
  
  # If no data mapping or plot configuration is input, use default
  # metaData <- metaData %||% metaDataHelper(data)
  dataMapping <- dataMapping %||% ObsVsPredDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% ObsVsPredPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )
  
  validateIsOfType(dataMapping, ObsVsPredDataMapping)
  validateIsOfType(plotConfiguration, ObsVsPredPlotConfiguration)
  
  plotObject <- plotObject %||% initializePlot(plotConfiguration)
  
  # Add obs vs pred lines
  plotObject <- plotConfiguration$addObsVsPredLines(plotObject, data, dataMapping)
    
  # Add scatter data
  plotObject <- plotConfiguration$addObsVsPred(plotObject, data, metaData, dataMapping)
    
  # Add smoothers
  if(!is.null(dataMapping$smoother)){
    plotObject <- plotConfiguration$addSmoother(plotObject, data, metaData, dataMapping)
  }
  
  plotObject <- plotConfiguration$legend$setPlotLegend(plotObject)
  
  return(plotObject)
}
