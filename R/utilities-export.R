#' @title setPlotExport
#' @description Set plot export properties
#' @param plotObject Graphical object created from ggplot
#' @param format file format of the exported plot
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height`
#' @return ggplot object with updated labels
#' @export
setPlotExport <- function(plotObject, format = NULL, width = NULL, height = NULL, units = NULL) {
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

#' @title setPlotExportFormat
#' @description Set plot export properties
#' @param plotObject Graphical object created from ggplot
#' @param format file format of the exported plot
#' @return ggplot object with updated labels
#' @export
setPlotExportFormat <- function(plotObject, format = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(format, nullAllowed = TRUE)

  newPlotObject <- plotObject
  newPlotObject$plotConfiguration <- plotObject$plotConfiguration$clone(deep = TRUE)

  # R6 class not cloned will spread modifications into newPlotObject$plotConfiguration$yAxis
  export <- newPlotObject$plotConfiguration$export
  export$format <- format %||% export$format

  return(newPlotObject)
}

#' @title setPlotExportSize
#' @description Set plot export properties
#' @param plotObject Graphical object created from ggplot
#' @param width plot width in `unit`
#' @param height plot height in `unit`
#' @param units units of `width` and `height`
#' @return ggplot object with updated labels
#' @export
setPlotExportSize <- function(plotObject, width = NULL, height = NULL, units = NULL) {
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

#' @title exportPlot
#' @description Save a `ggplot` object according to its export properties
#' @param plotObject Graphical object created from ggplot
#' @param name name of exported file without extension
#' @export
#' @import ggplot2
exportPlot <- function(plotObject, name = NULL) {
  validateIsOfType(plotObject, "ggplot")
  validateIsString(name, nullAllowed = TRUE)

  format <- plotObject$plotConfiguration$export$format
  width <- plotObject$plotConfiguration$export$width
  height <- plotObject$plotConfiguration$export$height
  units <- plotObject$plotConfiguration$export$units

  filename <- ifnotnull(
    name,
    paste0(name, ".", format),
    getUniqueExportFileName(tlfEnv$defaultExportName, format, NULL)
  )
  ggplot2::ggsave(filename, plot = plotObject, width = width, height = height, units = units)
}

getUniqueExportFileName <- function(name, format, uniqueNumber) {
  filename <- ifnotnull(
    uniqueNumber,
    paste0(name, "-", uniqueNumber, ".", format),
    paste0(name, ".", format)
  )
  if (file.exists(filename)) {
    uniqueNumber <- ifnotnull(uniqueNumber, uniqueNumber + 1, 1)
    filename <- getUniqueExportFileName(name, format, uniqueNumber)
  }
  return(filename)
}


#' @title exportPlotConfigurationCode
#' @description Export plot configuration as R code
#' @param plotConfiguration A `PlotConfiguration` object
#' @return R code to recreate the plot configuration as character
#' @export
exportPlotConfigurationCode <- function(plotConfiguration) {
  validateIsOfType(plotConfiguration, "PlotConfiguration")
  plotConfigurationClass <- class(plotConfiguration)[1]

  initializationCode <- c(
    "# Initialize the PlotConfiguration object",
    paste0("plotConfiguration <- ", plotConfigurationClass, "$new()")
  )

  labelsCode <- "# Define/Overwrite PlotConfiguration labels properties"
  for (label in c("title", "subtitle", "xlabel", "ylabel")) {
    for (property in c("text", "font$color", "font$size", "font$fontFace", "font$angle")) {
      labelText <- paste0("plotConfiguration$labels$", label, "$", property)
      labelValue <- eval(parse(text = labelText))
      labelsCode <- c(
        labelsCode,
        as.character(parseValueToObject(labelText, labelValue))
      )
    }
  }

  backgroundCode <- "# Define/Overwrite PlotConfiguration background properties"
  for (property in c("text", "font$color", "font$size", "font$fontFace", "font$angle")) {
    labelText <- paste0("plotConfiguration$background$watermark$", property)
    labelValue <- eval(parse(text = labelText))
    backgroundCode <- c(
      backgroundCode,
      as.character(parseValueToObject(labelText, labelValue))
    )
  }
  for (backgroundElement in c("plot", "panel", "xAxis", "yAxis", "xGrid", "yGrid")) {
    for (property in c("color", "size", "linetype", "fill")) {
      backgroundText <- paste0("plotConfiguration$background$", backgroundElement, "$", property)
      backgroundValue <- eval(parse(text = backgroundText))
      backgroundCode <- c(
        backgroundCode,
        as.character(parseValueToObject(backgroundText, backgroundValue))
      )
    }
  }

  axesCode <- "# Define/Overwrite PlotConfiguration axes properties"
  for (axisElement in c("xAxis", "yAxis")) {
    for (property in c("font$color", "font$size", "font$fontFace", "font$angle", "limits", "scale", "ticklabels", "ticks")) {
      axesText <- paste0("plotConfiguration$", axisElement, "$", property)
      axesValue <- eval(parse(text = axesText))
      axesCode <- c(
        axesCode,
        as.character(parseValueToObject(axesText, axesValue))
      )
    }
  }

  aestheticSelectionCode <- "# Define/Overwrite PlotConfiguration aesthetics selection properties"
  for (aestheticElement in c("points", "lines", "ribbons", "errorbars")) {
    for (property in c("color", "size", "linetype", "shape", "fill", "alpha")) {
      aestheticText <- paste0("plotConfiguration$", aestheticElement, "$", property)
      aestheticValue <- eval(parse(text = aestheticText))
      aestheticSelectionCode <- c(
        aestheticSelectionCode,
        as.character(parseValueToObject(aestheticText, aestheticValue))
      )
    }
  }

  legendCode <- "# Define/Overwrite PlotConfiguration legend properties"
  for (property in c("position", "title", "background$fill", "background$color", "background$size", "background$linetype", "titleFont$color", "titleFont$size", "titleFont$fontFace", "titleFont$angle", "font$color", "font$size", "font$fontFace", "font$angle")) {
    legendText <- paste0("plotConfiguration$legend$", property)
    legendValue <- eval(parse(text = legendText))
    legendCode <- c(
      legendCode,
      as.character(parseValueToObject(legendText, legendValue))
    )
  }

  classSpecificCode <- NULL
  if (plotConfigurationClass %in% "BoxWhiskerPlotConfiguration") {
    classSpecificCode <- c(
      "# Define/Overwrite properties specific to BoxWhisker plots",
      paste0("plotConfiguration$outliers <- ", plotConfiguration$outliers)
    )
  }
  if (plotConfigurationClass %in% "TornadoPlotConfiguration") {
    classSpecificCode <- "# Define/Overwrite Pproperties specific to Tornado plots"
    for (property in c("bar", "dodge", "colorPalette")) {
      classSpecificText <- paste0("plotConfiguration$", property)
      classSpecificValue <- eval(parse(text = classSpecificText))
      classSpecificCode <- c(
        classSpecificCode,
        as.character(parseValueToObject(classSpecificText, classSpecificValue))
      )
    }
  }

  plotConfigurationCode <- c(
    initializationCode, "\n",
    labelsCode, "\n",
    backgroundCode, "\n",
    axesCode, "\n",
    legendCode, "\n",
    aestheticSelectionCode, "\n",
    classSpecificCode
  )

  return(plotConfigurationCode)
}
