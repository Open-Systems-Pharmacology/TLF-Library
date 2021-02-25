messages <- list(
  errorWrongType = function(objectName, type, expectedType, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    expectedTypeMsg <- paste0(expectedType, collapse = ", or ")

    paste0(
      callingFunction, ": argument '", objectName,
      "' is of type '", type, "', but expected '", expectedTypeMsg, "'!", optionalMessage
    )
  },

  errorDifferentLength = function(objectNames, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]

    paste0(
      callingFunction, ": Arguments '", objectNames,
      "' must have the same length, but they don't!", optionalMessage
    )
  },

  errorWrongLength = function(object, nbElements, optionalMessage = NULL) {
    # Name of the calling function
    callingFunctions <- sys.calls()
    callingFunction <- sys.call(-length(callingFunctions) + 1)[[1]]
    paste0(
      callingFunction, ": Object should be of length '", nbElements, "', but is of length '", length(object), "' instead. ", optionalMessage
    )
  },

  errorEnumNotAllNames = "The enumValues has some but not all names assigned. They must be all assigned or none assigned",

  errorValueNotInEnum = function(enum, value) {
    paste0("Value '", value, "' is not in defined enumeration values: '", paste0(names(enum), collapse = ", "), "'.")
  },

  errorNotIncluded = function(values, parentValues) {
    paste0("Values '", paste0(values, collapse = ", "), "' are not in included in parent values: '", paste0(parentValues, collapse = ", "), "'.")
  },

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
