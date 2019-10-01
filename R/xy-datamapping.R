#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    plotPropertiesNames = NULL,
    color = NULL,
    size = NULL,
    shape = NULL,
    linetype = NULL,
    data = NULL,

    initialize = function(x, y,
                              color = NULL,
                              size = NULL,
                              shape = NULL,
                              linetype = NULL,
                              data = NULL,
                              metaData = NULL) {
      self$x <- x
      self$y <- y

      self$plotPropertiesNames <- c("color", "size", "shape", "linetype")

      self$color <- color
      self$size <- size
      self$shape <- shape
      self$linetype <- linetype


      if (!is.null(data)) {
        self$data <- self$getMappedData(data, metaData)
      }
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      plotProperties <- list()

      for (propertyName in self$plotPropertiesNames) {
        plotProperties[[propertyName]] <- getDefaultCaptions(
          data = data,
          metaData = metaData,
          variableList = self[[propertyName]]
        )
      }

      self$data <- data.frame(
        "x" = x, "y" = y,
        "color" = plotProperties$color,
        "shape" = plotProperties$shape,
        "size" = plotProperties$size,
        "linetype" = plotProperties$linetype
      )
      return(self$data)
    }
  )
)


#' @title XYEDataMapping
#' @docType class
#' @description Abstract class for X Y Error Mapping
#' @export
XYEDataMapping <- R6::R6Class(
  "XYEDataMapping",
  inherit = XYDataMapping,
  public = list(
    error = NULL,
    errorMin = NULL,
    errorMax = NULL,

    # Example of how to do some other stuff
    initialize = function(x, y,
                              error = NULL,
                              errorMin = NULL,
                              errorMax = NULL, ...) {
      super$initialize(x, y, ...)
      self$error <- error
      self$errorMin <- error %||% errorMin
      self$errorMax <- error %||% errorMax
    },

    getMapData = function(data, metaData) {
      x <- data[, self$x]
      y <- data[, self$y]
      errorMin <- data[, self$errorMin]
      errorMax <- data[, self$errorMax]

      color <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$color
      )

      shape <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$shape
      )

      size <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$size
      )

      return(list(
        "x" = x, "y" = y, "ymin" = y - errorMin, "ymax" = y + errorMax,
        "color" = color, "shape" = shape, "size" = size
      ))
    }
  )
)

#' @title XRangeDataMapping
#' @docType class
#' @description Abstract class for X Range Mapping
#' @export
XRangeDataMapping <- R6::R6Class(
  "XRangeDataMapping",
  inherit = XYDataMapping,
  public = list(
    yMin = NULL,
    yMax = NULL,

    # Example of how to do some other stuff
    initialize = function(x, yMin, yMax) {
      super$initialize(x, y = NULL, ...)
      self$yMin <- yMin
      self$yMax <- yMax
    },

    getMappedData = function(data, metaData) {
      x <- data[, self$x]
      yMin <- data[, self$yMin]
      yMin <- data[, self$yMin]

      color <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$color
      )

      return(list("x" = x, "yMin" = yMin, "yMax" = yMax, "color" = color))
    }
  )
)


#' @title getDefaultCaptions
#' @param data input data.frame with variables to group by
#' @param metaData input data.frame with variables to group by
#' @param variableList groups as factor levels
#' @param sep characters separating variables in caption
#' @description
#' getDefaultCaptions create a new column that groups the grouping variable
#' @return groupingVariable
#' @export
#'
getDefaultCaptions <- function(data, metaData, variableList = colnames(data), sep = "-") {

  # Check that the grouping is in the list of data variables
  stopifnot(variableList %in% colnames(data))

  groupingVariable <- asLegendCaptionSubset(
    data[, variableList[1]],
    metaData[[variableList[1]]]
  )

  # Paste the consecutively the variable names
  for (variable in tail(variableList, -1)) {
    groupingVariable <- paste(
      groupingVariable,
      asLegendCaptionSubset(
        data[, variable],
        metaData[[variable]]
      ),
      sep = sep
    )
  }

  if (length(groupingVariable) == 0) {
    groupingVariable <- 1
  }
  groupingVariable <- as.factor(groupingVariable)
  return(groupingVariable)
}


asLegendCaptionSubset <- function(data, metaData) {
  CaptionSubset <- as.character(data)

  # If numeric create a character as rounded numeric + unit from metadata
  if ("numeric" %in% class(data)) {
    CaptionSubset <- paste(as.character(round(data)), metaData$unit, sep = "")
  }

  return(CaptionSubset)
}
