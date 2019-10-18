#' @title GroupMapping
#' @docType class
#' @description  Abstract class for Grouping
#' @export
GroupMapping <- R6::R6Class(
  "GroupMapping",
  public = list(
    group = NULL,
    label = NULL,

    initialize = function(group, label = NULL) {
      validateIsOfType(group, c("data.frame", "character"), nullAllowed = TRUE)

      self$group <- group

      if (is.data.frame(group)) {
        # For data.frame, label is last column as default
        self$label <- utils::tail(names(group), 1)
      }
      if (is.character(group)) {
        self$label <- paste(group, collapse = "-")
      }

      self$label <- label %||% self$label
    },

    getCaptions = function(data, metaData = NULL) {
      validateIsOfType(self$group, c("data.frame", "character"), nullAllowed = TRUE)
      if (is.data.frame(self$group)) {
        captions <- getCustomCaptions(data, self$group)
      }
      if (is.character(self$group)) {
        captions <- getDefaultCaptions(data, metaData, variableList = self$group)
      }
      return(captions)
    }
  )
)
