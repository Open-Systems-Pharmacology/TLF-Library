#' Get Data Grouping
#'
#' @param data input data.frame with variable to group
#' @param grouping vector of variable names to group by
#' @description
#' Function to group data and create groups according to variable
#' So far, the grouping is performed only along one variable
#' @return data with groups as factor
#' @export
#'
getGrouping <- function(data, grouping) {
  # Check that grouping variable are in the data
  stopifnot(grouping %in% names(data))
  if (length(grouping) > 1) {
    stop("Grouping with more than one variable is not currently handled")
  }

  if (is.null(grouping)) {
    # If grouping is NULL, send 1 group only
    groupVariable <- factor(1)
  } else {
    groupVariable <- factor(data[, grouping])
  }
  return(groupVariable)
}
