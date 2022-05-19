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
    return(outputIfEqual)
  }
  return(outputIfNotEqual)
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
    return(outputIfIncluded)
  }
  return(outputIfNotIncluded)
}

# Because collate put tlf-env and themes before utils,
# The curretnTheme is defined here: after the definition of %||%
tlfEnv$currentTheme <- Theme$new()
# Default theme is minimal when package is loaded
useMinimalTheme()
