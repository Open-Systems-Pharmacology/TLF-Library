#' @title getSymmetricLimits
#' @description Get symmetric limits from a set of values
#' @param values numeric values
#' @return An array of 2 symmetric values equally distant from 0
#' @export
#' @examples
#' getSymmetricLimits(seq(-3, 8))
#'
getSymmetricLimits <- function(values) {
  validateIsNumeric(values, nullAllowed = TRUE)
  # Remove Inf and NA values
  values <- values[!is.infinite(values)]
  values <- values[!is.na(values)]
  if (isEmpty(values)) {
    return(NULL)
  }
  return(c(-max(abs(values)), max(abs(values))))
}


#' @title getSameLimits
#' @description Get same limits from multiple sets of values
#' @param ... numeric values
#' @return An array of 2 values
#' @export
#' @examples
#' getSameLimits(seq(-3, 8), seq(-12, 4))
#'
getSameLimits <- function(...) {
  values <- c(...)
  validateIsNumeric(values, nullAllowed = TRUE)
  # Remove Inf and NA values
  values <- values[!is.infinite(values)]
  values <- values[!is.na(values)]
  if (isEmpty(values)) {
    return(NULL)
  }
  return(c(min(values), max(values)))
}
