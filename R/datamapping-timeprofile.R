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
                              dataFrame = NULL,
                              yMin = NULL,
                              yMax = NULL,
                              LLOQ = NULL) {
      if (is.null(y)) {
        if (is.null(yMin) && is.null(yMax)) {
          stop("Either y or yMin and yMax must be defined for TimeProfileDataMapping")
        }
      }

      super$initialize(x, y, groupings)

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

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (!is.null(self$groupings[[groupType]]$group)) {
          grouping <- self$groupings[[groupType]]
          self$data[, grouping$label] <- grouping$getCaptions(data, metaData)
        }
      }
      return(self$data)
    }
  )
)
