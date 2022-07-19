#' @title .parseVariableToObject
#' @description Create an expression of type `objectName$variableName <- variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `objectName$variableName <- variableName %||% objectName$variableName`
#' @return An expression to `eval()`
#' @keywords internal
.parseVariableToObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName, " %||% ", objectName, "$", variableName)))
  }
  return(parse(text = paste0(objectName, "$", variableName, " <- ", variableName)))
}

#' @title .parseVariableFromObject
#' @description Create an expression of type `variableName <- objectName$variableName`
#' @param objectName Name of the object whose field is updated
#' @param variableName Name of the variable and field of `objectName`
#' @param keepIfNull logical `variableName <- objectName$variableName %||% variableName`
#' @return An expression to `eval()`
#' @keywords internal
.parseVariableFromObject <- function(objectName, variableName, keepIfNull = FALSE) {
  if (keepIfNull) {
    return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName, " %||% ", variableName)))
  }
  return(parse(text = paste0(variableName, " <- ", objectName, "$", variableName)))
}

#' @title .parseValueToObject
#' @description Create an expression of type `objectName <- value`
#' @param objectName Name of the object to update
#' @param value Value of the variable `objectName`
#' @return An expression to `eval()`
#' @keywords internal
.parseValueToObject <- function(objectName, value) {
  if (isEmpty(value)) {
    return(parse(text = paste0(objectName, " <- NULL")))
  }
  if (isOfType(value, "character")) {
    return(parse(text = paste0(objectName, ' <- "', value, '"')))
  }
  return(parse(text = paste0(objectName, " <- ", value)))
}

#' @title .parseCheckPlotInputs
#' @description Create an expression that checks usual plot inputs
#' @param plotType Type of plot (e.g. "PKRatio" for plotPKRatio)
#' @return An expression to `eval()`
#' @keywords internal
.parseCheckPlotInputs <- function(plotType) {
  c(
    expression(validateIsOfType(data, "data.frame")),
    expression(validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)),
    parse(text = paste0('if(nrow(data)==0){warning(messages$errorNrowData("', plotType, ' plot")); return(plotObject)}')),
    parse(text = paste0("dataMapping <- dataMapping %||% ", plotType, "DataMapping$new(data=data)")),
    parse(text = paste0(
      "plotConfiguration <- plotConfiguration %||% ",
      plotType, "PlotConfiguration$new(data=data, metaData=metaData, dataMapping=dataMapping)"
    )),
    parse(text = paste0('validateIsOfType(dataMapping, "', plotType, 'DataMapping")')),
    parse(text = paste0('validateIsOfType(plotConfiguration, "', plotType, 'PlotConfiguration")'))
  )
}

#' @title .parseUpdateAxes
#' @description Create an expression that updates the plot axes
#' @return An expression to `eval()`
#' @keywords internal
.parseUpdateAxes <- function() {
  # Try is used to prevent crashes in the final plot due to ggplot2 peculiarities regarding scale functions
  c(
    expression(try(suppressMessages(plotObject <- setXAxis(plotObject)))),
    expression(try(suppressMessages(plotObject <- setYAxis(plotObject))))
  )
}

#' @title .parseUpdateAestheticProperty
#' @description Create an expression that updates the aesthetic properties based on
#' the information of `PlotConfiguration`
#' @param aestheticProperty Name of aesthetic property as defined in `AestheticProperties`
#' @param plotConfigurationProperty Name of PlotConfiguration property as defined in `AestheticProperties`
#' @return An expression to `eval()`
#' @keywords internal
.parseUpdateAestheticProperty <- function(aestheticProperty, plotConfigurationProperty) {
  c(
    parse(text = paste0(aestheticProperty, 'Variable <- gsub("`", "", mapLabels$', aestheticProperty, ")")),
    parse(text = paste0(aestheticProperty, "Length <- length(unique(mapData[, ", aestheticProperty, "Variable]))")),
    # Update the property using ggplot `scale` functions
    parse(text = paste0(
      "suppressMessages(plotObject <- plotObject + ggplot2::scale_", aestheticProperty, "_manual(",
      "values=.getAestheticValues(n=", aestheticProperty, "Length,",
      "selectionKey=plotConfiguration$", plotConfigurationProperty, "$", aestheticProperty,
      ',aesthetic = "', aestheticProperty, '")))'
    )),
    # remove the legend of aesthetic if default unmapped aesthetic
    parse(text = paste0("if(isIncluded(", aestheticProperty, 'Variable, "legendLabels")){plotObject <- plotObject + ggplot2::guides(', aestheticProperty, " = 'none')}"))
  )
}

#' @title .parseAddScatterLayer
#' @description Create an expression that adds scatter plot layer
#' @return An expression to `eval()`
#' @keywords internal
.parseAddScatterLayer <- function() {
  expression({
    plotObject <- plotObject +
      ggplot2::geom_point(
        data = mapData,
        mapping = ggplot2::aes_string(
          x = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          shape = mapLabels$shape
        ),
        size = .getAestheticValues(n = 1, selectionKey = plotConfiguration$points$size, position = 0, aesthetic = "size"),
        alpha = .getAestheticValues(n = 1, selectionKey = plotConfiguration$points$alpha, aesthetic = "alpha"),
        na.rm = TRUE,
        show.legend = TRUE
      )
  })
}

#' @title .parseAddLineLayer
#' @description Create an expression that adds scatter plot layer
#' TODO: create a vignette explaining how argument `lines` in dataMapping is related to this
#' @param type one of "horizontal", "vertical" or "diagonal"
#' Note that for "diagonal", geom_abline is used.
#' `value` of intercept is taken as is for linear scale but corresponds to the log of `value` for log scale.
#'  For instance, intercept = c(-1, 0, 1) with log scale actually means that the line will go through c(0.1, 1, 10)
#'  because c(-1, 0, 1) = log10(c(0.1, 1, 10)).
#' @param value value of xintercept or yintercept
#' @param position line position for aesthetic properties
#' @return An expression to `eval()`
#' @keywords internal
.parseAddLineLayer <- function(type, value, position) {
  parse(text = paste0(
    "plotObject <- plotObject + ",
    switch(type,
      "horizontal" = paste0("ggplot2::geom_hline(yintercept = ", value, ","),
      "vertical" = paste0("ggplot2::geom_vline(xintercept = ", value, ","),
      "diagonal" = paste0("ggplot2::geom_abline(slope=1, intercept = ", value, ","),
      "ddiHorizontal" = paste0("ggplot2::geom_abline(slope=0, intercept = ", value, ",")
    ),
    "color=.getAestheticValues(n=1,selectionKey=plotConfiguration$lines$color,position=", position, ',aesthetic="color"),',
    "linetype=.getAestheticValues(n=1,selectionKey=plotConfiguration$lines$linetype,position=", position, ',aesthetic="linetype"),',
    "alpha=.getAestheticValues(n=1,selectionKey=plotConfiguration$lines$alpha,position=", position, ',aesthetic="alpha"),',
    "size=.getAestheticValues(n=1,selectionKey=plotConfiguration$lines$size,position=", position, ', aesthetic="size"))'
  ))
}

#' @title .parseAddUncertaintyLayer
#' @description Create an expression that adds errorbars if uncertainty is included in dataMapping
#' @return An expression to `eval()`
#' @keywords internal
.parseAddUncertaintyLayer <- function(direction = "vertical") {
  parse(text = paste0(
    "plotObject <- plotObject +",
    # Plot error bars from xmin/ymin to x/y
    # If lower value is negative and plot is log scaled,
    # Upper bar will still be plotted
    "ggplot2::geom_linerange(",
    "data = mapData,",
    "mapping = aes_string(",
    switch(direction,
      "vertical" = "x = mapLabels$x, ymin = mapLabels$ymin, ymax = mapLabels$y,",
      "horizontal" = "y = mapLabels$y, xmin = mapLabels$xmin, xmax = mapLabels$x,"
    ),
    "color = mapLabels$color",
    "),",
    'size = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),',
    'linetype = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype"),',
    'alpha = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$alpha, aesthetic = "alpha"),',
    "na.rm = TRUE,",
    "show.legend = FALSE",
    ") + ",
    "ggplot2::geom_linerange(",
    "data = mapData,",
    "mapping = aes_string(",
    switch(direction,
      "vertical" = "x = mapLabels$x, ymin = mapLabels$y, ymax = mapLabels$ymax,",
      "horizontal" = "y = mapLabels$y, xmin = mapLabels$x, xmax = mapLabels$xmax,"
    ),
    "color = mapLabels$color",
    "),",
    'size = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$size, position = 0, aesthetic = "size"),',
    'linetype = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$linetype, aesthetic = "linetype"),',
    'alpha = .getAestheticValues(n = 1, selectionKey = plotConfiguration$errorbars$alpha, aesthetic = "alpha"),',
    "na.rm = TRUE,",
    "show.legend = FALSE",
    ")"
  ))
}
