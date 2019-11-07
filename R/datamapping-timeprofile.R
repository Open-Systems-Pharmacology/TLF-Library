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
                              yMin = NULL,
                              yMax = NULL,
                              groupMapping = NULL,
                              LLOQ = NULL, ...) {
      if (is.null(y)) {
        if (is.null(yMin) && is.null(yMax)) {
          stop("Either y or yMin and yMax must be defined for TimeProfileDataMapping")
        }
      }

      super$initialize(x = x, y = y, groupMapping = groupMapping)

      self$yMin <- yMin
      self$yMax <- yMax
      self$LLOQ <- LLOQ
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
    },

    checkMapData = function(data, metaData = NULL) {
      validateMapping(self$x, data)
      validateMapping(self$y, data, nullAllowed = TRUE)
      validateMapping(self$yMin, data, nullAllowed = TRUE)
      validateMapping(self$yMax, data, nullAllowed = TRUE)

      self$data <- data[, c(self$x, self$y, self$yMin, self$yMax)]

      # All possible Groupings are listed in the enum LegendTypes
      for (groupType in LegendTypes) {
        if (!is.null(self$groupMapping[[groupType]]$group)) {
          grouping <- self$groupMapping[[groupType]]

          groupVariables <- grouping$group
          if (isOfType(groupVariables, "data.frame")) {
            # Last group variable is the label in group data.frames
            # and need to be removed from the check
            groupVariables <- names(groupVariables)
            groupVariables <- utils::head(groupVariables, -1)
          }
          validateMapping(groupVariables, data)

          self$data[, grouping$label] <- grouping$getCaptions(data, metaData)
        }
      }
      # Dummy variable for default aesthetics
      self$data$defaultAes <- factor("")
      return(self$data)
    }
  )
)
