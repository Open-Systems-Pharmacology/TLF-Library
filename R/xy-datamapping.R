#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    data = NULL,

    initialize = function(x = NULL,
                              y = NULL,
                              data = NULL,
                              metaData = NULL) {
      self$x <- NULL
      self$y <- NULL

      if (!is.null(data)) {
        self$x <- names(data)[1]
        self$y <- names(data)[2]
      }
      if (!is.null(x)) {
        self$x <- x
      }
      if (!is.null(y)) {
        self$y <- y
      }

      if (is.null(self$x) || is.null(self$y)) {
        stop("Undefined mapping for x or y")
      }
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- data.frame("x" = x, "y" = y)
      return(self$data)
    }
  )
)

#' @title Grouping
#' @docType class
#' @description  Abstract Grouping class for Mapping colors, shape to groups
#' @export
Grouping <- R6::R6Class(
  "Grouping",
  public = list(
    group = NULL,
    groupName = NULL,

    initialize = function(group = NULL,
                              groupName = NULL,
                              data = NULL,
                              metaData = metaData) {
      if (!is.null(data)) {
        self$group <- names(data)
      }
      self$group <- group
      self$groupName <- groupName %||% paste(self$group, collapse = "-")
    },

    getGrouping = function(data, metaData = NULL) {
      grouping <- getDefaultCaptions(
        data = data,
        metaData = metaData,
        variableList = self$group
      )
      return(grouping)
    },

    addGroupingToData = function(data, metaData = NULL) {
      grouping <- self$getGrouping(data, metaData)
      groupingName <- paste(self$group, collapse = "-")

      data[, goupingName] <- grouping

      return(data)
    }
  )
)

#' @title XYGDataMapping
#' @docType class
#' @description  Abstract class for X Y Group Mapping
#' @export
XYGDataMapping <- R6::R6Class(
  "XYGDataMapping",
  inherit = XYDataMapping,
  public = list(
    groupings = NULL, # List of Groupings that are R6 Classes as defined by Abdullah
    groupingNames = NULL,

    initialize = function(x = NULL,
                              y = NULL,
                              groupings = NULL,
                              data = NULL,
                              metaData = NULL) {
      super$initialize(
        x = x,
        y = y,
        data = data,
        metaData = metaData
      )

      self$groupings <- NULL

      if (is.character(groupings)) {
        groupings <- list(groupings)
      }

      self$groupingNames <- names(groupings)

      if (!is.null(groupings)) {
        for (groupingsIndex in seq(1, length(groupings))) {
          if (is.null(self$groupingNames[groupingsIndex]) || is.na(self$groupingNames[groupingsIndex])) {
            self$groupingNames[groupingsIndex] <- paste(groupings[[groupingsIndex]], collapse = "-")
          }

          self$groupings[[groupingsIndex]] <- Grouping$new(
            group = groupings[[groupingsIndex]],
            groupName = self$groupingNames[groupingsIndex],
            data = data,
            metaData = metaData
          )
        }
        names(self$groupings) <- self$groupingNames
      }
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- data[, self$y]

      self$data <- cbind.data.frame(x, y)

      if (!is.null(self$groupings)) {
        for (groupingsIndex in seq(1, length(self$groupings))) {
          self$data[, self$groupingNames[[groupingsIndex]]] <- self$groupings[[groupingsIndex]]$getGrouping(data, metaData)
        }
      }

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

  # Loop on the variableList except first one
  # pasting as a single data.frame column the association of names in all selected variables
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
  captionSubset <- as.character(data)

  # If numeric create a character as rounded numeric + unit from metadata
  if ("numeric" %in% class(data)) {
    captionSubset <- paste(as.character(round(data)), metaData$unit, sep = "")
  }

  return(captionSubset)
}
