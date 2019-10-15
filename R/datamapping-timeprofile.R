#' @title TimeProfileDataMapping
#' @docType class
#' @description  Data Mapping for Time Profile
#' @export
TimeProfileDataMapping <- R6::R6Class(
  "TimeProfileDataMapping",
  inherit = XYGDataMapping,

  public = list(
    yMin = NULL,
    yMax = NULL,
    LLOQ = NULL,

    initialize = function(x,
                              y = NULL,
                              groupings = NULL,
                              groupingNames = NULL,
                              dataFrame = NULL,
                              yMin = NULL,
                              yMax = NULL,
                              LLOQ = NULL) {
      if (is.null(y)) {
        if (is.null(yMin) && is.null(yMax)) {
          stop("Either y or yMin and yMax must be defined for TimeProfileDataMapping")
        }
      }

      super$initialize(x, y, groupings, groupingNames)

      self$LLOQ <- LLOQ
      self$yMin <- yMin
      self$yMax <- yMax
    },

    getMapData = function(data, metaData = NULL) {
      x <- data[, self$x]
      y <- ifnotnull(self$y, data[, self$y], NULL)
      yMin <- ifnotnull(self$yMin, data[, self$yMin], NULL)
      yMax <- ifnotnull(self$yMax, data[, self$yMax], NULL)

      self$data <- as.data.frame(cbind(x, y, yMin, yMax))

      # For each mapped grouping, get the corresponding data frame
      # Defined by grouping data frame if available or by grouping Name
      if (!is.null(self$groupings)) {
        for (groupingsIndex in seq(1, length(self$groupings))) {
          self$data[, self$groupingNames[[groupingsIndex]]] <- ifnotnull(self$groupings[[groupingsIndex]]$groupingDataFrame, getCustomCaptions(data, self$groupings[[groupingsIndex]]$groupingDataFrame), getDefaultCaptions(data, metaData, variableList = self$groupings[[groupingsIndex]]$groupingName[[1]]))
        }
      }

      return(self$data)
    }
  )
)
