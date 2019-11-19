#' @title BoxWhiskerDataMapping
#' @docType class
#' @description  Class for Box Whisker Mapping
#' @export
BoxWhiskerDataMapping <- R6::R6Class(
  "BoxWhiskerDataMapping",
  inherit = XYDataMapping,
  public = list(
    quantiles = NULL,
    outliers = NULL,
    ymin = NULL,
    lower = NULL,
    middle = NULL,
    upper = NULL,
    ymax = NULL,

    initialize = function(...,
                              ymin = NULL,
                              lower = NULL,
                              middle = NULL,
                              upper = NULL,
                              ymax = NULL,
                              quantiles = c(0.05, 0.25, 0.5, 0.75, 0.95),
                              outliers = NULL) {
      super$initialize(...)
      
      validateIsOfLength(quantiles, 5)
      self$quantiles <- sort(quantiles)
      self$outliers <- outliers

      self$ymin <- ymin %||% self$quantiles[1]
      self$lower <- lower %||% self$quantiles[2]
      self$middle <- middle %||% self$quantiles[3]
      self$upper <- upper %||% self$quantiles[4]
      self$ymax <- ymax %||% self$quantiles[5]

      self$quantiles <- c(self$ymin, self$lower, self$middle, self$upper, self$ymax)
    },

    getQuantiles = function(data) {
      # Redfine group and y while removing NA values
      group <- ifnotnull(
        self$x,
        data[!is.na.data.frame(data[, self$y]), self$x],
        1
      )
      y <- data[!is.na.data.frame(data[, self$y]), self$y]

      quantiles <- tapply(y, group, FUN = function(x) {
        as.numeric(quantile(x, probs = self$quantiles))
      })
      # As a data.frame summary row names are already group names
      quantiles <- as.data.frame(t(sapply(quantiles, FUN = rbind)))

      # Re-label variables
      names(quantiles) <- as.character(self$quantiles)

      # Pass the row names as a factor variable in data.frame
      ifnotnull(
        self$x,
        quantiles[, self$x] <- row.names(quantiles)
      )
      # Dummy variable for aesthetics
      quantiles$defaultAes <- factor("")

      return(quantiles)
    }
  )
)
