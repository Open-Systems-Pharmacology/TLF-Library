#' @title Grouping
#' @docType class
#' @description  Abstract class for Grouping. Maps variable(s) to a group
#' @field group Mapping of variable(s) to its group
#' Can be as a vector of variable names or as a data.frame
#' @field label Name of the variable that will be defined by the Grouping (optional)
#' @section Methods:
#' \describe{
#' \item{new(group, label = NULL)}{Initialize Grouping. group is either a vector of variable names or a data.frame linking variables to their captions.}
#' \item{getCaptions(data, metaData = NULL)}{Create captions variable as a data.frame column whose header is label.
#' Variable is of type factor to define groups.}
#' }
#' @export
Grouping <- R6::R6Class(
  "Grouping",
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
