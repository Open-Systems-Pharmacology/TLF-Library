#' @title .addScatterLayer
#' @description Add scatter points layer of a molecule plot
#' @param plotObject A `ggplot` object
#' @param data A data.frame with labels mapped to properties and obtained from a `DataMapping` object
#' @param mapLabels List of mapped label names passed to `ggplot2::aes_string`
#' @return A `ggplot` object
#' @keywords internal
.addScatterLayer <- function(plotObject, data, mapLabels) {
  aestheticValues <- .getAestheticValuesFromConfiguration(
    n = 1,
    position = 0,
    plotConfigurationProperty = plotObject$plotConfiguration$points,
    propertyNames = c("size", "alpha")
  )

  plotObject <- plotObject +
    ggplot2::geom_point(
      data = data,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        y = mapLabels$y,
        color = mapLabels$color,
        shape = mapLabels$shape
      ),
      size = aestheticValues$size,
      alpha = aestheticValues$alpha,
      na.rm = TRUE,
      show.legend = TRUE
    )
  return(plotObject)
}

#' @title .addErrorbarLayer
#' @description Add errorbar layer of a molecule plot
#' @param plotObject A `ggplot` object
#' @param data A data.frame with labels mapped to properties and obtained from a `DataMapping` object
#' @param mapLabels List of mapped label names passed to `ggplot2::aes_string`
#' @return A `ggplot` object
#' @keywords internal
.addErrorbarLayer <- function(plotObject, data, mapLabels, direction = "vertical") {
  aestheticValues <- .getAestheticValuesFromConfiguration(
    n = 1,
    position = 0,
    plotConfigurationProperty = plotObject$plotConfiguration$errorbars,
    propertyNames = c("size", "linetype", "alpha")
  )

  plotObject <- switch(direction,
    "vertical" = plotObject +
      ggplot2::geom_linerange(
        data = data,
        mapping = ggplot2::aes_string(
          x = mapLabels$x,
          # If lower value is negative and plot is log scaled,
          # Upper bar will still be plotted
          ymin = mapLabels$ymin,
          ymax = mapLabels$y,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = aestheticValues$size,
        linetype = aestheticValues$linetype,
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::geom_linerange(
        data = data,
        mapping = ggplot2::aes_string(
          x = mapLabels$x,
          # If lower value is negative and plot is log scaled,
          # Upper bar will still be plotted
          ymin = mapLabels$y,
          ymax = mapLabels$ymax,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = aestheticValues$size,
        linetype = aestheticValues$linetype,
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      # Add lower cap to error bar
      # If lower value is negative and plot is log scaled,
      # Upper bar cap will still be plotted
      ggplot2::geom_point(
        data = data,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$ymin,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = tlfEnv$defaultErrorbarCapSize,
        shape = "_",
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      # Add upper cap to error bar
      # If lower value is negative and plot is log scaled,
      # Upper bar cap will still be plotted
      ggplot2::geom_point(
        data = data,
        mapping = aes_string(
          x = mapLabels$x,
          y = mapLabels$ymax,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = tlfEnv$defaultErrorbarCapSize,
        shape = "_",
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ),
    "horizontal" = plotObject +
      ggplot2::geom_linerange(
        data = data,
        mapping = ggplot2::aes_string(
          # If lower value is negative and plot is log scaled,
          # Upper bar will still be plotted
          xmin = mapLabels$xmin,
          xmax = mapLabels$x,
          y = mapLabels$y,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = aestheticValues$size,
        linetype = aestheticValues$linetype,
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      ggplot2::geom_linerange(
        data = data,
        mapping = ggplot2::aes_string(
          # If lower value is negative and plot is log scaled,
          # Upper bar will still be plotted
          xmin = mapLabels$x,
          xmax = mapLabels$xmax,
          y = mapLabels$y,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = aestheticValues$size,
        linetype = aestheticValues$linetype,
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      # Add lower cap to error bar
      # If lower value is negative and plot is log scaled,
      # Upper bar cap will still be plotted
      ggplot2::geom_point(
        data = data,
        mapping = aes_string(
          x = mapLabels$xmin,
          y = mapLabels$y,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = tlfEnv$defaultErrorbarCapSize,
        shape = "|",
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      ) +
      # Add upper cap to error bar
      # If lower value is negative and plot is log scaled,
      # Upper bar cap will still be plotted
      ggplot2::geom_point(
        data = data,
        mapping = aes_string(
          x = mapLabels$xmax,
          y = mapLabels$y,
          color = mapLabels$color,
          group = mapLabels$shape
        ),
        size = tlfEnv$defaultErrorbarCapSize,
        shape = "|",
        alpha = aestheticValues$alpha,
        na.rm = TRUE,
        show.legend = FALSE
      )
  )
  return(plotObject)
}

#' @title .addLineLayer
#' @description Add line layer of a molecule plot
#' @param plotObject A `ggplot` object
#' @param type one of "horizontal", "vertical" or "diagonal"
#' Note that for "diagonal", geom_abline is used.
#' `value` of intercept is taken as is for linear scale but corresponds to the log of `value` for log scale.
#'  For instance, intercept = c(-1, 0, 1) with log scale actually means that the line will go through c(0.1, 1, 10)
#'  because c(-1, 0, 1) = log10(c(0.1, 1, 10)).
#' @param value value of xintercept or yintercept
#' @param position line position for aesthetic properties
#' @return A `ggplot` object
#' @keywords internal
.addLineLayer <- function(plotObject, type, value, position) {
  aestheticValues <- .getAestheticValuesFromConfiguration(
    n = 1,
    position = position,
    plotConfigurationProperty = plotObject$plotConfiguration$lines,
    propertyNames = c("color", "linetype", "size", "alpha")
  )

  plotObject <- plotObject + switch(type,
    "horizontal" = ggplot2::geom_hline(
      yintercept = value,
      color = aestheticValues$color,
      linetype = aestheticValues$linetype,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    ),
    "vertical" = ggplot2::geom_vline(
      xintercept = value,
      color = aestheticValues$color,
      linetype = aestheticValues$linetype,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    ),
    "diagonal" = ggplot2::geom_abline(
      slope = 1,
      intercept = value,
      color = aestheticValues$color,
      linetype = aestheticValues$linetype,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    ),
    "ddiHorizontal" = ggplot2::geom_abline(
      slope = 0,
      intercept = value,
      color = aestheticValues$color,
      linetype = aestheticValues$linetype,
      alpha = aestheticValues$alpha,
      size = aestheticValues$size
    )
  )
  return(plotObject)
}
