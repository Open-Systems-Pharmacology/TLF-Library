#' @keywords internal
.validateMapping <- function(mapping, data, nullAllowed = FALSE) {
  if (nullAllowed && is.null(mapping)) {
    return()
  }

  if (isOfType(mapping, "numeric")) {
    if (ncol(data) >= mapping) {
      return()
    }
    stop(messages$errorExceedLength(mapping, ncol(data)))
  }
  if (isOfType(mapping, "character")) {
    variableNames <- names(data)
    validateIsIncluded(mapping, variableNames)
    return()
  }

  stop(messages$errorWrongType(mapping, class(mapping), c("numeric", "character")))
}

#' Check that 2 conflicting inputs can't be provided at the same time
#'
#' @param eitherInput Input or list of inputs
#' @param orInput Input or list of inputs
#' @keywords internal
.validateEitherOrNullInput <- function(eitherInput, orInput) {
  # Convert input to list of input if not already as list
  if (!isOfType(eitherInput, "list")) {
    eitherInput <- list(eitherInput)
  }
  if (!isOfType(orInput, "list")) {
    orInput <- list(orInput)
  }

  # False if one element from input is not null
  if (as.logical(min(sapply(eitherInput, is.null))) || as.logical(min(sapply(orInput, is.null)))) {
    return()
  }
  stop(messages$errorConflictingInput(names(eitherInput), names(orInput)))
}

#' Check that at least one log tick is included in limits
#'
#' @param limits An array of numeric values
#' @param scale Name of log scale: `Scaling$log` for log10 scale, `Scaling$ln` for logarithmic scale
#' @keywords internal
.isLogTicksIncludedInLimits <- function(limits, scale) {
  minLimit <- min(limits, na.rm = TRUE)
  maxLimit <- max(limits, na.rm = TRUE)
  exponentValues <- switch(scale,
    "log" = seq(floor(log10(minLimit)), ceiling(log10(maxLimit))),
    "ln" = seq(floor(log(minLimit)), ceiling(log(maxLimit)))
  )
  logTicks <- rep(seq(1, 9), length(exponentValues)) * switch(scale,
    "log" = 10^rep(exponentValues, each = 9),
    "ln" = exp(rep(exponentValues, each = 9))
  )
  return(sum(isBetween(logTicks, minLimit, maxLimit)) > 0)
}
