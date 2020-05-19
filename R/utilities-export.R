#'@title setPlotExport
#'@description Set plot export properties
#'@param plotObject Graphical object created from ggplot
#'@param format file format of the exported plot
#'@param width plot width in `unit`
#'@param height plot height in `unit`
#'@param units units of `width` and `height`
#'@return ggplot object with updated labels
#'@export
setPlotExport <- function(plotObject, format = NULL, width = NULL, height = NULL, units = NULL){
  validateIsOfType(plotObject, "ggplot")
  validateIsNumeric(c(width, height), nullAllowed = TRUE)
  validateIsString(c(format, units), nullAllowed = TRUE)
  
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)
  
  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  export <- newPlotObject$plotConfiguration$export
  export$format <- format %||% export$format
  export$width <- width %||% export$width
  export$height <- height %||% export$height
  export$units <- units %||% export$units
  
  return(newPlotObject)
}

#'@title setPlotExportFormat
#'@description Set plot export properties
#'@param plotObject Graphical object created from ggplot
#'@param format file format of the exported plot
#'@return ggplot object with updated labels
#'@export
setPlotExportFormat <- function(plotObject, format = NULL){
  validateIsOfType(plotObject, "ggplot")
  validateIsString(format, nullAllowed = TRUE)
  
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)
  
  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  export <- newPlotObject$plotConfiguration$export
  export$format <- format %||% export$format
  
  return(newPlotObject)
}

#'@title setPlotExportSize
#'@description Set plot export properties
#'@param plotObject Graphical object created from ggplot
#'@param width plot width in `unit`
#'@param height plot height in `unit`
#'@param units units of `width` and `height`
#'@return ggplot object with updated labels
#'@export
setPlotExportSize <- function(plotObject, width = NULL, height = NULL, units = NULL){
  validateIsOfType(plotObject, "ggplot")
  validateIsNumeric(c(width, height), nullAllowed = TRUE)
  validateIsString(units, nullAllowed = TRUE)
  
  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)
  
  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  export <- newPlotObject$plotConfiguration$export
  export$width <- width %||% export$width
  export$height <- height %||% export$height
  export$units <- units %||% export$units
  
  return(newPlotObject)
}

#'@title exportPlot
#'@description Save a `ggplot` object according to its export properties
#'@param plotObject Graphical object created from ggplot
#'@param name name of exported file without extension
#'@export
#'@import ggplot2
exportPlot <- function(plotObject, name = NULL){
  validateIsOfType(plotObject, "ggplot")
  validateIsString(name, nullAllowed = TRUE)
  
  format <- plotObject$plotConfiguration$export$format
  width <- plotObject$plotConfiguration$export$width
  height <- plotObject$plotConfiguration$export$height
  units <- plotObject$plotConfiguration$export$units
  
  filename <- ifnotnull(name, 
                        paste0(name, ".", format),
                        getUniqueExportFileName(tlfEnv$defaultExportName, format, NULL))
  ggplot2::ggsave(filename, plot=plotObject, width=width, height=height, units=units)
}

getUniqueExportFileName <- function(name, format, uniqueNumber){
  filename <- ifnotnull(uniqueNumber,
                        paste0(name, "-", uniqueNumber, ".", format),
                        paste0(name, ".", format))
  if(file.exists(filename)){
    uniqueNumber <- ifnotnull(uniqueNumber, uniqueNumber+1, 1)
    filename<-getUniqueExportFileName(name, format, uniqueNumber)
  }
  return(filename)
}