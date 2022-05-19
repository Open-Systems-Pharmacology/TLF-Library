#' @title plotHistogram
#' @description
#' Producing Histograms
#'
#' @inheritParams addScatter
#' @param bins Number or edges of bins.
#' If `bins` is provided as a single numeric values, `bin` corresponds to number of bins.
#' The bin edges are then equally spaced within the range of data.
#' If `bins` is provided as an array of numeric values, `bin` corresponds to their edges.
#' @param binwidth Numerical value of defining the width of each bin.
#' If defined, `binwidth` can overwrite `bins` if `bins` was not provided or simply provided as a single value.
#' @param stack Logical defining for multiple histograms if their bars are stacked
#' @param distribution Name of distribution to fit to the data.
#' Only 2 distributions are currently available: `"normal"` and `"logNormal"`
#' @param dataMapping 
#' A `HistogramDataMapping` object mapping `x` and aesthetic groups to their variable names of `data`.
#' @param plotConfiguration 
#' An optional `HistogramPlotConfiguration` object defining labels, grid, background and watermark.
#' @return A `ggplot` object
#'
#' @references For examples, see:
#' <https://www.open-systems-pharmacology.org/TLF-Library/articles/histogram.html>
#'
#' @export
#' @family molecule plots
#' @examples 
#' # Produce histogram of normally distributed data
#' plotHistogram(x = rnorm(100))
#' 
#' # Produce histogram of normally distributed data with many bins
#' plotHistogram(x = rlnorm(100), bins = 21)
#' 
#' # Produce histogram of fitted normally distributed data
#' plotHistogram(x = rlnorm(100), distribution = "normal")
#' 
#' 
plotHistogram <- function(data = NULL,
                          metaData = NULL,
                          x = NULL,
                          dataMapping = NULL,
                          bins = NULL,
                          binwidth = NULL,
                          stack = NULL,
                          distribution = NULL,
                          plotConfiguration = NULL,
                          plotObject = NULL) {
  validateIsNumeric(bins, nullAllowed = TRUE)
  validateIsNumeric(binwidth, nullAllowed = TRUE)
  validateIsLogical(stack, nullAllowed = TRUE)
  validateIsIncluded(distribution, c("normal", "logNormal", "none"), nullAllowed = TRUE)

  if (is.null(data)) {
    validateIsNumeric(x)
    data <- data.frame(x = x)
    dataMapping <- dataMapping %||% HistogramDataMapping$new(
      x = ifNotNull(x, "x"),
      data = data
    )
  }
  dataMapping <- dataMapping %||% HistogramDataMapping$new(data = data)
  plotConfiguration <- plotConfiguration %||% HistogramPlotConfiguration$new(
    data = data,
    metaData = metaData,
    dataMapping = dataMapping
  )

  validateIsOfType(dataMapping, "HistogramDataMapping")
  validateIsOfType(plotConfiguration, "HistogramPlotConfiguration")
  validateIsOfType(data, "data.frame")
  validateIsOfType(plotObject, "ggplot", nullAllowed = TRUE)

  # Overwrites plotConfiguration and dataMapping if some inputs are not null
  dataMapping$stack <- stack %||% dataMapping$stack
  dataMapping$distribution <- distribution %||% dataMapping$distribution
  dataMapping$bins <- bins %||% dataMapping$bins
  dataMapping$binwidth <- binwidth %||% dataMapping$binwidth
  
  plotObject <- plotObject %||% initializePlot(plotConfiguration)

  if (nrow(data) == 0) {
    warning(messages$errorNrowData("Histogram"))
    return(plotObject)
  }

  # Get transformed data from mapping and convert labels into characters usable by aes_string
  mapData <- dataMapping$checkMapData(data)
  mapLabels <- getAesStringMapping(dataMapping)

  position <- ggplot2::position_nudge()
  if (dataMapping$stack) {
    position <- ggplot2::position_stack()
  }
  
  edges <- NULL
  if(length(dataMapping$bins)>1){
    edges <- dataMapping$bins
  }

  plotObject <- plotObject +
    ggplot2::geom_histogram(
      data = mapData,
      mapping = ggplot2::aes_string(
        x = mapLabels$x,
        fill = mapLabels$fill
      ),
      position = position,
      bins = dataMapping$bins,
      binwidth = dataMapping$binwidth,
      breaks = edges,
      size = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$size, position = 0, aesthetic = "size"),
      color = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$color, position = 0, aesthetic = "color"),
      alpha = getAestheticValues(n = 1, selectionKey = plotConfiguration$ribbons$alpha, position = 0, aesthetic = "alpha")
    )

  # If distribution is provided by dataMapping, get median and distribution of the data
  fitData <- getDistributionFit(mapData, dataMapping)
  fitMedian <- getDistributionMed(mapData, dataMapping)
  
  if(!isOfLength(fitData, 0)){
    plotObject <- plotObject +
      ggplot2::geom_line(
        data = fitData,
        mapping = ggplot2::aes_string(
          x = "x",
          y = "y",
          color = "legendLabels",
          linetype = "legendLabels"
        ),
        size = getAestheticValues(n = 1, selectionKey = plotConfiguration$lines$size, position = 0, aesthetic = "size")
      )
  }

  # Include vertical lines
  for (lineIndex in seq_along(fitMedian)) {
    # position corresponds to the number of layer lines already added
    eval(parseAddLineLayer("vertical", fitMedian[lineIndex], lineIndex - 1))
  }

  # Define fill based on plotConfiguration$points properties
  eval(parseUpdateAestheticProperty(AestheticProperties$fill, "ribbons"))
  eval(parseUpdateAestheticProperty(AestheticProperties$color, "lines"))
  eval(parseUpdateAestheticProperty(AestheticProperties$linetype, "lines"))
  eval(parseUpdateAxes())
  return(plotObject)
}


#' @title getDistributionFit
#' @description Get a data.frame from the fit of a distribution provided in `dataMapping`
#' If `normal` distribution is selected, its mean is plotted
#' If `logNormal` distribution is selected, its mode is plotted
#' @param data data.frame containing the data to be used for the plot
#' @param dataMapping A `HistogramDataMapping` object
#' The object defines the distribution to be fitted and the option `stack`.
#' If the bars are stacked, the fit will account for the final histogram
#' @return A data.frame with `x`, `y` and `legendLabels`
#' @keywords internal
getDistributionFit <- function(data, dataMapping) {
  if (isIncluded(dataMapping$distribution, "none")) {
    return()
  }
  x <- data[, dataMapping$x]
  # Get array of x values
  xFit <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 1e3)
    #switch(
    #dataMapping$distribution,
    #"normal" = seq(-max(abs(x), na.rm = TRUE), max(abs(x), na.rm = TRUE), length.out = 1e3),
    #"logNormal" = seq(0, max(abs(x), na.rm = TRUE), length.out = 1e3)
  #)

  # Get binwidth
  bins <- dataMapping$bins
  if (length(bins) > 1) {
    bins <- length(bins) - 1
  }
  binwidth <- (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)) / bins
  binwidth <- dataMapping$binwidth %||% binwidth

  if (dataMapping$stack) {
    dataFit <- data.frame(
      x = xFit,
      y = length(x) * binwidth * switch(
        dataMapping$distribution,
        "normal" = stats::dnorm(xFit, mean = mean(x, na.rm = TRUE), sd = stats::sd(x, na.rm = TRUE)),
        "logNormal" = stats::dlnorm(xFit, meanlog = mean(log(x), na.rm = TRUE), sdlog = stats::sd(log(x), na.rm = TRUE))
      ),
      legendLabels = paste0(dataMapping$distribution, " distribution fit")
    )
    return(dataFit)
  }

  dataFit <- NULL
  for (groupLevel in levels(data$legendLabels)) {
    selectedGroup <- data$legendLabels %in% groupLevel
    dataFit <- rbind.data.frame(
      dataFit,
      data.frame(
        x = xFit,
        y = length(x[selectedGroup]) * binwidth * switch(
          dataMapping$distribution,
          "normal" = stats::dnorm(
            xFit,
            mean = mean(x[selectedGroup], na.rm = TRUE),
            sd = stats::sd(x[selectedGroup], na.rm = TRUE)
          ),
          "logNormal" = stats::dlnorm(
            xFit,
            meanlog = mean(log(x[selectedGroup]), na.rm = TRUE),
            sdlog = stats::sd(log(x[selectedGroup]), na.rm = TRUE)
          )
        ),
        legendLabels = groupLevel
      )
    )
  }
  return(dataFit)
}

#' @title getDistributionMed
#' @description Get an array of values from the fit of a distribution provided in `dataMapping`
#' If `normal` distribution is selected, its mean is plotted
#' If `logNormal` distribution is selected, its mode is plotted
#' @param data data.frame containing the data to be used for the plot
#' @param dataMapping A `HistogramDataMapping` object
#' The object defines the distribution to be fitted and the option `stack`.
#' If the bars are stacked, the fit will account for the final histogram
#' @return Numeric values for vertical lines
#' @keywords internal
getDistributionMed <- function(data, dataMapping) {
  if (isIncluded(dataMapping$distribution, "none")) {
    return()
  }
  x <- data[, dataMapping$x]
  if (dataMapping$stack) {
    return(
      switch(
        dataMapping$distribution,
        "normal" = mean(x, na.rm = TRUE),
        "logNormal" = exp(mean(log(x), na.rm = TRUE) - stats::var(log(x), na.rm = TRUE))
      )
    )
  }

  xintercept <- NULL
  for (groupLevel in levels(data$legendLabels)) {
    selectedGroup <- data$legendLabels %in% groupLevel
    xintercept <- c(
      xintercept,
      switch(
        dataMapping$distribution,
        "normal" = mean(x[selectedGroup], na.rm = TRUE),
        "logNormal" = exp(mean(log(x[selectedGroup]), na.rm = TRUE) - stats::var(log(x[selectedGroup]), na.rm = TRUE))
      )
    )
  }
  return(xintercept)
}
