#' @title BoxWhiskerDataMapping
#' @docType class
#' @description  Data Mapping for Box Whisker Plots
#' @field x Name of x variable to map
#' @field y Name of y variable to map
#' @field ymin Name of function used for calculating lower whisker
#' @field lower Name of function used for calculating lower line of box
#' @field middle Name of function used for calculating middle line
#' @field upper Name of function used for calculating upper line of box
#' @field ymax Name of function used for calculating upper whisker
#' @field minOutlierLimit Name of function used for calculating lower outlier limit
#' @field maxOutlierLimit Name of function used for calculating upper outlier limit
#' @section Methods:
#' \describe{
#' \item{new(x,y,ymin = tlfStatFunctions$`Percentile-2.5%`,lower = tlfStatFunctions$`Percentile-25%`,
#' middle = tlfStatFunctions$`Percentile-50%`,upper = tlfStatFunctions$`Percentile-75%`,ymax = tlfStatFunctions$`Percentile-97.5%`,
#' minOutlierLimit = tlfStatFunctions$`median-1.5IQR`,maxOutlierLimit = tlfStatFunctions$`median+1.5IQR`,...)}{
#' Initialize BoxWhiskerDataMapping. ymin, lower, middle, upper, ymax, minOutlierLimit, maxOutlierLimit inputs are names of functions}
#' \item{checkMapData(data, metaData = NULL)}{Check data mapping is correct. Create output data.frame with map data only.}
#' \item{getBoxWhiskers(data)}{Check data mapping is correct. Create output data.frame with map data only.}
#' \item{getOutliers(data)}{Check data mapping is correct. Create output data.frame with map data only.}
#' }
#' @export
BoxWhiskerDataMapping <- R6::R6Class(
  "BoxWhiskerDataMapping",
  inherit = XYDataMapping,
  public = list(
    outlierLimits = NULL,
    boxWhiskerLimits = NULL,

    initialize = function(x,
                              y,
                              ymin = tlfStatFunctions$`Percentile-2.5%`,
                              lower = tlfStatFunctions$`Percentile-25%`,
                              middle = tlfStatFunctions$`Percentile-50%`,
                              upper = tlfStatFunctions$`Percentile-75%`,
                              ymax = tlfStatFunctions$`Percentile-97.5%`,
                              minOutlierLimit = tlfStatFunctions$`median-1.5IQR`,
                              maxOutlierLimit = tlfStatFunctions$`median+1.5IQR`,
                              ...) {
      super$initialize(x = x, y = y, ...)

      self$boxWhiskerLimits <- c(ymin, lower, middle, upper, ymax)
      self$outlierLimits <- c(minOutlierLimit, maxOutlierLimit)
    },

    getBoxWhiskerLimits = function(data) {

      # Transform names into functions for aggregation summary
      boxWhiskerLimitsFunctions <- sapply(self$boxWhiskerLimits, match.fun)

      # Use aggregation summary to get box specific values
      summaryObject <- AggregationSummary$new(
        data = data,
        xColumnNames = self$x,
        yColumnNames = self$y,
        aggregationFunctionsVector = boxWhiskerLimitsFunctions,
        aggregationFunctionNames = c("ymin", "lower", "middle", "upper", "ymax")
      )

      boxWhiskerLimits <- summaryObject$dfHelper

      # Dummy variable for aesthetics
      boxWhiskerLimits$defaultAes <- factor("")

      return(boxWhiskerLimits)
    },

    getOutliers = function(data) {

      # Transform names into functions for aggregation summary
      outlierLimitsFunctions <- sapply(self$outlierLimits, match.fun)

      # Use aggregation summary to get outliers boundaries specific values
      summaryObject <- AggregationSummary$new(
        data = data,
        xColumnNames = self$x,
        yColumnNames = self$y,
        aggregationFunctionsVector = outlierLimitsFunctions,
        aggregationFunctionNames = c("minOutlierLimit", "maxOutlierLimit")
      )

      outlierLimits <- summaryObject$dfHelper

      # Merge outlier limits to data
      outliers <- merge.data.frame(data, outlierLimits)

      # Create the outliers variables by flagging which are lower or higher than limits
      outliers[, "minOutliers"] <- outliers[, self$y]
      outliers[, "maxOutliers"] <- outliers[, self$y]

      minOutliersFlag <- outliers[, self$y] < outliers[, "minOutlierLimit"]
      maxOutliersFlag <- outliers[, self$y] > outliers[, "maxOutlierLimit"]

      outliers$minOutliers[!minOutliersFlag] <- NA
      outliers$maxOutliers[!maxOutliersFlag] <- NA

      # Dummy variable for aesthetics
      outliers$defaultAes <- factor("")

      return(outliers)
    }
  )
)
