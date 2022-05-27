messages <- list(
  errorExceedLength = function(values, parentLengths) {
    paste0("Values '", paste0(values, collapse = ", "), "' exceed variable length : '", paste0(parentLengths, collapse = ", "), "'.")
  },
  errorConflictingInput = function(eitherInput, orInput) {
    paste0(
      "Conflicting inputs provided. Inputs '", paste0(eitherInput, collapse = ", "),
      "' and '", paste0(orInput, collapse = ", "), "' can't be provided at the same time."
    )
  },
  errorNrowData = function(plotName) {
    paste0("nrow(data)=0. ", plotName, " layer could not be added.")
  }
)
