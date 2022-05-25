
asPlotShape <- function(shapes) {
  ggplotShapes <- NULL
  for (shape in shapes) {
    ggplotShape <- as.character(shape)
    if (isIncluded(ggplotShape, names(Shapes))) {
      ggplotShape <- Shapes[[shape]]
    }
    ggplotShapes <- c(ggplotShapes, ggplotShape)
  }
  return(ggplotShapes)
}

#' @title getAestheticValues
#' @description Get aesthetic values (e.g color, shape, linetype) based on a selected strategy
#' @param n integer defining size of returned aesthetic vector
#' @param selectionKey value of aesthetic to be returned or key function from enum `AestheticSelectionKeys`
#' @param position integer defining the current position in the aesthetic map
#' @param aesthetic name of aesthetic property as defined in enum `AestheticProperties`
#' @return Vector of aesthetics
#' @keywords internal
getAestheticValues <- function(n, selectionKey = NA, position = 0, aesthetic = "color") {
  validateIsIncluded(aesthetic, AestheticProperties)
  # Load aesthetics from current `Theme` object
  map <- tlfEnv$currentTheme$aestheticMaps[[aesthetic]]
  if (isIncluded(aesthetic, AestheticProperties$shape)) {
    map <- asPlotShape(map)
  }
  # next is necessary as `next` otherwise R crashes
  if (isIncluded(selectionKey, AestheticSelectionKeys$`next`)) {
    return(getNextAestheticValues(n, position, map))
  }
  if (isIncluded(selectionKey, AestheticSelectionKeys$same)) {
    return(getSameAestheticValues(n, position, map))
  }
  if (isIncluded(selectionKey, AestheticSelectionKeys$first)) {
    return(getFirstAestheticValues(n, map))
  }
  if (isIncluded(selectionKey, AestheticSelectionKeys$reset)) {
    return(getResetAestheticvalues(n, map))
  }
  # TO DO: Add wrappers for validation
  # which could translate some selection keys for ggplot 2
  if (isIncluded(aesthetic, AestheticProperties$shape)) {
    return(asPlotShape(getNextAestheticValues(n, position = position, map = selectionKey)))
  }
  if (isIncluded(aesthetic, c(AestheticProperties$size, AestheticProperties$alpha))) {
    return(as.numeric(getNextAestheticValues(n, position = position, map = selectionKey)))
  }
  # If selection key is a specific value/set of values, it becomes the map
  return(getNextAestheticValues(n, position = position, map = selectionKey))
}

#' @title getNextAestheticValues
#' @description Get the next aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`
#' @param n integer defining size of returned aesthetic vector
#' @param position integer defining at which position to look for aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
#' @keywords internal
getNextAestheticValues <- function(n, position = 0, map) {
  # Get the map indices of aesthtic values to be output
  aesPositions <- seq(position + 1, position + n)
  if (n <= 0) {
    return(NULL)
  }

  # In case the map is not long enough to get all the indices
  # the indices have to go start back from 1 using modelu function
  mapSize <- length(map)
  aesPositions <- ((aesPositions - 1) %% mapSize) + 1
  return(map[aesPositions])
}

#' @title getSameAestheticValues
#' @description Get the same aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`
#' @param n integer defining size of returned aesthetic vector
#' @param position integer defining at which position to look for aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
#' @keywords internal
getSameAestheticValues <- function(n, position = 0, map) {
  # In case the map is not long enough to get the indices in position
  # the indices have to go start back from 1 using modelu function
  mapSize <- length(map)
  aesPosition <- (position %% mapSize) + 1
  if (n <= 0) {
    return(NULL)
  }

  # Get the map indices of aesthtic values to be output
  aesPositions <- rep(aesPosition, n)
  return(map[aesPositions])
}

#' @title getResetAestheticvalues
#' @description Get the aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`.
#' Reset the value every time it is used.
#' @param n integer defining size of returned aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
#' @keywords internal
getResetAestheticvalues <- function(n, map) {
  return(getNextAestheticValues(n, position = 0, map))
}

#' @title getFirstAestheticValues
#' @description Get the next aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`
#' @param n integer defining size of returned aesthetic vector
#' @param position integer defining at which position to look for aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
#' @keywords internal
getFirstAestheticValues <- function(n, map) {
  return(getSameAestheticValues(n, position = 0, map))
}
