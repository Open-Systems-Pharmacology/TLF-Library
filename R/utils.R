`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

#' Shortkey checking if argument 1 is not null,
#' output the argument 2 if not null, or output argument 3 otherwise
#'
#' @title ifnotnull
#' @param inputToCheck argument 1
#' @param outputIfNotNull argument 2
#' @param outputIfNull argument 3
#' @return outputIfNotNull if inputToCheck is not null, outputIfNull otherwise
#' @description
#' Check if inputToCheck is not null, if so output outputIfNotNull,
#' otherwise, output outputIfNull
#' @keywords internal
ifnotnull <- function(inputToCheck, outputIfNotNull, outputIfNull = NULL) {
  if (!is.null(inputToCheck)) {
    outputIfNotNull
  } else {
    outputIfNull
  }
}

#' Shortkey checking if arguments 1 and 2 are equal,
#' output argument 3 if equal, or output argument 4 otherwise
#'
#' @title ifEqual
#' @param x argument 1
#' @param y argument 2
#' @param outputIfEqual argument 3
#' @param outputIfNotEqual argument 4
#' @return outputIfEqual if x=y, outputIfNotEqual otherwise
#' @description
#' Check if x=y, if so output outputIfEqual,
#' otherwise, output outputIfNotEqual
#' @keywords internal
ifEqual <- function(x, y, outputIfEqual, outputIfNotEqual = NULL) {
  if (x == y) {
    outputIfEqual
  } else {
    outputIfNotEqual
  }
}

#' Shortkey checking if arguments 1 is included in 2,
#' output argument 3 if included, or output argument 4 otherwise
#'
#' @title ifIncluded
#' @param x argument 1
#' @param y argument 2
#' @param outputIfIncluded argument 3
#' @param outputIfNotIncluded argument 4
#' @return outputIfIncluded if x=y, outputIfNotIncluded otherwise
#' @description
#' Check if x is in y, if so output outputIfIncluded,
#' otherwise, output outputIfNotIncluded
#' @keywords internal
ifIncluded <- function(x, y, outputIfIncluded, outputIfNotIncluded = NULL) {
  if (isIncluded(x, y)) {
    outputIfIncluded
  } else {
    outputIfNotIncluded
  }
}

# Because collate put tlf-env and themes before utils,
# The curretnTheme is defined here: after the definition of %||%
tlfEnv$currentTheme <- Theme$new()
