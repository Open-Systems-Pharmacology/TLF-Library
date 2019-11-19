#' @title BoxWhiskerDataMapping
#' @docType class
#' @description  Class for Box Whisker Mapping
#' @export
BoxWhiskerDataMapping <- R6::R6Class(
  "BoxWhiskerDataMapping",
  inherit = XYDataMapping,
  public = list(
    percentiles = NULL,
    outliers = NULL,
    ymin = NULL,
    lower = NULL,
    middle = NULL,
    upper = NULL,
    ymax = NULL,

    initialize = function(ymin = NULL,
                              lower = NULL,
                              middle = NULL,
                              upper = NULL,
                              ymax = NULL,
                              percentiles = 100 * c(0.05, 0.25, 0.5, 0.75, 0.95),
                              outliers = NULL,
                              ...) {
      super$initialize(...)

      validateIsOfLength(percentiles, 5)
      self$percentiles <- sort(percentiles)
      self$outliers <- outliers

      self$ymin <- ymin %||% self$percentiles[1]
      self$lower <- lower %||% self$percentiles[2]
      self$middle <- middle %||% self$percentiles[3]
      self$upper <- upper %||% self$percentiles[4]
      self$ymax <- ymax %||% self$percentiles[5]

      self$percentiles <- c(self$ymin, self$lower, self$middle, self$upper, self$ymax)
    },

    getPercentiles = function(data) {
      # Redfine group and y while removing NA values
      group <- ifnotnull(
        self$x,
        data[!is.na.data.frame(data[, self$y]), self$x],
        1
      )
      y <- data[!is.na.data.frame(data[, self$y]), self$y]

      # probs and percentiles are defined between 0 and 1
      # while self$percentiles between 0 and 100
      percentiles <- tapply(y, group, FUN = function(x) {
        as.numeric(quantile(x, probs = self$percentiles / 100))
      })
      # As a data.frame summary row names are already group names
      percentiles <- as.data.frame(t(sapply(percentiles, FUN = rbind)))

      # Re-label variables
      names(percentiles) <- as.character(self$percentiles)

      # Pass the row names as a factor variable in data.frame
      ifnotnull(
        self$x,
        percentiles[, self$x] <- row.names(percentiles)
      )
      # Dummy variable for aesthetics
      percentiles$defaultAes <- factor("")

      return(percentiles)
    }
  )
)
