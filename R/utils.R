#' Shortkey checking if argument 1 is not null, 
#' output argument 1 if not null, or output argument 2 otherwise 
#' 
#' @param lhs
#' @param rhs
#' @return lhs if lhs is not null, rhs otherwise
#' @description 
#' Check if lhs argument is not null, output lhs if not null,
#' output rhs otherwise
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
#' @param inputToCheck
#' @param outputIfNotNull
#' @param outputIfNull
#' @return outputIfNotNull if inputToCheck is not null, outputIfNull otherwise
#' @description 
#' Check if inputToCheck is not null, if so output outputIfNotNull,
#' otherwise, output outputIfNull
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
#' @param x
#' @param y
#' @param outputIfEqual
#' @param outputIfNotEqual
#' @return outputIfEqual if x=y, outputIfNotEqual otherwise
#' @description 
#' Check if x=y, if so output outputIfEqual,
#' otherwise, output outputIfNotEqual
ifequal <- function(x, y, outputIfEqual, outputIfNotEqual = NULL) {
  if (x == y) {
    outputIfEqual
  } else {
    outputIfNotEqual
  }
}

#' Shortkey checking if arguments 1 is included in 2, 
#' output argument 3 if included, or output argument 4 otherwise
#' 
#' @param x
#' @param y
#' @param outputIfIncluded
#' @param outputIfNotIncluded
#' @return outputIfIncluded if x=y, outputIfNotIncluded otherwise
#' @description 
#' Check if x is in y, if so output outputIfIncluded,
#' otherwise, output outputIfNotIncluded
ifIncluded <- function(x, y, outputIfIncluded, outputIfNotIncluded = NULL) {
  if (isIncluded(x, y)) {
    outputIfIncluded
  } else {
    outputIfNotIncluded
  }
}
