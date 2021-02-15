#' @title AestheticProperties
#' @description Enum of aesthetic property names of `ggplot2`
AestheticProperties <- enum(c(
  "color",
  "fill",
  "size",
  "shape",
  "linetype",
  "alpha"
))

#' @title Linetypes
#' @description Enum of `ggplot2` linetypes
#' @export
Linetypes <- enum(c(
  "solid",
  "longdash",
  "dotted",
  "dashed",
  "twodash",
  "dotdash",
  "blank"
))

#' @title Shapes
#' @description List of some `ggplot2` shapes
#' @export
Shapes <- list(
  "square" = 15,
  "dot" = 20,
  "circle" = 19,
  "diamond" = 18,
  "star" = 8,
  "plus" = 3,
  "cross" = 4,
  "triangle" = 17,
  "blank" = -2
)

asPlotShape <- function(shapes) {
  ggplotShapes <- NULL
  for (shape in shapes) {
    ggplotShape <- shape
    if (isOfType(shape, "character")) {
      validateIsIncluded(shape, names(Shapes))
      ggplotShape <- Shapes[[shape]]
    }
    ggplotShapes <- c(ggplotShapes, ggplotShape)
  }
  return(ggplotShapes)
}

#' @title ColorMaps
#' @description List with some color maps for `Theme` object
#' @export
ColorMaps <- list(
  default = c("#0078D7", "#D83B01", "#107C10", "#A80000", "#002050", "#B4009E"),
  grays = paste("gray", seq(0, 100, 10), sep = ""),
  prism = c("#FF0000", "#FF7F00", "#FFFF00", "#00FF00", "#0000FF", "#4B0082", "#8F00FF"),
  blot = c("blue", "magenta", "cyan", "green", "yellow", "red"),
  temperature = c("#262A76", "#234990", "#2F8AC3", "#26B0D2", "#FFC1CB", "#EB559", "#AE3535", "8E1F20")
)

#' @title AestheticSelectionKeys
#' @description List of some `ggplot2` shapes
#' @export
AestheticSelectionKeys <- enum(c(
  "next",
  "same",
  "first",
  "reset"
))

#' @title getAestheticValues
#' @description Get aesthetic values (e.g color, shape, linetype) based on a selected strategy
#' @param n integer defining size of returned aesthetic vector
#' @param selectionKey value of aesthetic to be returned or key function from enum `AestheticSelectionKeys`
#' @return Vector of aesthetics
getAestheticValues <- function(n, selectionKey = NA, position = 0, aesthetic = "color") {
  validateIsIncluded(aesthetic, AestheticProperties)
  # Load aesthetics from current `Theme` object
  map <- tlfEnv$currentTheme$aestheticMaps[[aesthetic]]
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
    return(asPlotShape(getSpecificAestheticValues(n, selectionKey)))
  }
  if (isIncluded(aesthetic, c(AestheticProperties$size, AestheticProperties$alpha))) {
    return(as.numeric(getSpecificAestheticValues(n, selectionKey)))
  }
  return(getSpecificAestheticValues(n, selectionKey))
}

#' @title getNextAestheticValues
#' @description Get the next aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`
#' @param n integer defining size of returned aesthetic vector
#' @param position integer defining at which position to look for aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
getNextAestheticValues <- function(n, position = 0, map) {
  # Get the map indices of aesthtic values to be output
  aesPositions <- seq(position + 1, position + n)

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
getSameAestheticValues <- function(n, position = 0, map) {
  # In case the map is not long enough to get the indices in position
  # the indices have to go start back from 1 using modelu function
  mapSize <- length(map)
  aesPosition <- (position %% mapSize) + 1

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
getResetAestheticvalues <- function(n, map) {
  return(getNextAestheticValues(n, position = 0, map))
}

#' @title getFirstAestheticValues
#' @description Get the next aesthetic values (e.g color, shape, linetype) from an aesthetic map defined in `Theme`
#' @param n integer defining size of returned aesthetic vector
#' @param position integer defining at which position to look for aesthetic vector
#' @param map Aesthetic (e.g. color, shape, linetype) map from `Theme` object.
#' @return Vector of aesthetics
getFirstAestheticValues <- function(n, map) {
  return(getSameAestheticValues(n, position = 0, map))
}

#' @title getSpecificAestheticValues
#' @description Get specific aesthetic values (e.g color, shape, linetype)
#' @param n integer defining size of returned aesthetic vector
#' @param values values of the aesthetic to be returned
#' @return Vector of aesthetics
getSpecificAestheticValues <- function(n, values) {
  # Ensure that enough aesthetics are defined in case values is a vector
  aesValues <- rep(values, n)
  return(aesValues[1:n])
}
