checkIfNotNumeric <- function(vec, msg = "Input must be numeric.") {
  if (!is.numeric(vec)) {
    stop(msg)
  }
}


binNumericCol <- function(binLimits, currentDataCol) { # function to iterate through a numeric column and find elements that fall within bin limits.  Returns a logical column of the same size as the input data column.
  checkIfNotNumeric(currentDataCol, msg = "Dataframe column entries to be binned must be numeric.") # check that dataDf points to be binned are numeric
  sapply(binLimits, checkIfNotNumeric, msg = "Bin limits must be numeric") # check that all bin limits are numeric
  if (!(length(binLimits) == 2)) { # check that bin limits are of length 2.
    stop("Each element of bin limits list must be a vector of length 2.")
  }
  if (!(binLimits[2] > binLimits[1])) { # check that bin limits are in increasing order
    stop("Bin limits must be increasing.")
  }
  dataPointsInGrp <- sapply(currentDataCol, function(x) return((x >= binLimits[1]) & (x <= binLimits[2])))
  return(dataPointsInGrp)
}



findColumnEntriesInGroups <- function(dataDf, groupingDfRow) {
  groupingDataColumns <- dataDf[ colnames(groupingDfRow) ] # get columns from dataDf corresponding to column headings in groupingDf (excluding the last column heading in groupingDf)
  logicMatrix <- matrix(rep(FALSE, nrow(dataDf) * ncol(groupingDfRow)), nrow(dataDf))
  for (n in seq(1, ncol(groupingDfRow))) { # for each column of the groupingDf
    if (is.list(groupingDfRow[[n]])) { # case where a list is supplied for binning
      logicMatrix[, n] <- binNumericCol(groupingDfRow[[n]][[1]], groupingDataColumns[[n]]) # check if each column entry lies within bin limits.  If yes, return TRUE for that entry, else FALSE.
    } else { # case where there is no binning, only matching between caption dataframe entries and dataDf column entries
      logicMatrix[, n] <- sapply(groupingDataColumns[[n]], function(x) {
        return(x == groupingDfRow[[n]])
      }) # check if each column entry matches groupingDf element.  If yes, return TRUE for that entry, else FALSE.
    }
  }
  return(logicMatrix)
}


getCustomCaptions <- function(dataDf, groupingDf) {
  # dataDf is the original dataframe
  # groupingDf is a dataframe used to group dataDf
  # newColName is a default legend title for this grouping
  stopifnot(ncol(groupingDf) > 1)
  vecIndxGroupingDfRows <- seq(1, nrow(groupingDf)) # vector of factor levels associated with each caption
  groupingFactorColumn <- rep(0, nrow(dataDf)) # vector that is to be populated with factor levels that determine caption
  for (k in vecIndxGroupingDfRows) { # for each caption
    logicMatrix <- findColumnEntriesInGroups(dataDf, groupingDf[k, -ncol(groupingDf), drop = FALSE]) # call function to test if each column entry falls within the grouping
    for (m in seq(1, nrow(dataDf))) { # for each row of dataDf
      if (all(logicMatrix[m, ])) { # if entire dataDf row matches groupingDf row
        groupingFactorColumn[m] <- vecIndxGroupingDfRows[k] # set factor level in groupingFactorColumn to k
      }
    }
  }
  newCol <- as.factor(rep(NA, nrow(dataDf))) # add a factor column of NA to the dataframe dataDF.  For rows falling within the grouping, the NA will be modified to that grouping's caption.
  levels(newCol) <- groupingDf[[ncol(groupingDf)]] # Set factor levels of new column to be the captions of the grouping dataframe
  newCol[ groupingFactorColumn != 0 ] <- groupingDf[[ncol(groupingDf)]][ groupingFactorColumn[groupingFactorColumn != 0 ] ] # Add the appropriate caption to each column entry that correspondinds to a dataDf row that falls within groupings in groupingDf
  return(newCol)
}

#' @title getDefaultCaptions
#' @param data data.frame used for legend caption
#' @param metaData list of lists containing metaData on \code{data}
#' @param variableList ordered vector of variables used for specifying the caption
#' @param sep characters separating variables in caption
#' @description
#' Creates default legend captions by concatenating the values of
#' the \code{data} and \code{metaData} of \code{variableList} variables from \code{data}.
#' @return Factor levels corresponding to the legend captions
#' @export
#' @examples
#'
#' data <- data.frame(
#'   Population = c("Caucasian", "Asian", "Caucasian", "Asian"),
#'   Gender = c("Male", "Male", "Female", "Female"),
#'   Dose = c(50, 100, 100, 50),
#'   Compound = c("Midazolam", "Midazolam", "Midazolam", "Midazolam")
#' )
#'
#' metaData <- list(Dose = list(unit = "mg"))
#'
#' # Get captions using every variable of data
#' getDefaultCaptions(data, metaData)
#'
#' # Get captions using specific variables of data
#' getDefaultCaptions(data, metaData, variableList = c("Gender", "Population"))
#'
#' # Get captions separating variables witha space (character " ")
#' getDefaultCaptions(data, metaData, sep = " ")
getDefaultCaptions <- function(data, metaData, variableList = colnames(data), sep = "-") {

  # Check that the grouping is in the list of data variables
  stopifnot(variableList %in% colnames(data))

  groupingVariable <- asLegendCaptionSubset(
    data[, variableList[1]],
    metaData[[variableList[1]]]
  )

  # Loop on the variableList except first one
  # pasting as a single data.frame column the association of names in all selected variables
  for (variable in utils::tail(variableList, -1)) {
    groupingVariable <- paste(
      groupingVariable,
      asLegendCaptionSubset(
        data[, variable],
        metaData[[variable]]
      ),
      sep = sep
    )
  }

  if (length(groupingVariable) == 0) {
    groupingVariable <- 1
  }
  groupingVariable <- as.factor(groupingVariable)
  return(groupingVariable)
}


asLegendCaptionSubset <- function(data, metaData) {
  captionSubset <- as.character(data)

  # If numeric create a character as rounded numeric + unit from metadata
  if ("numeric" %in% class(data)) {
    captionSubset <- paste(as.character(round(data)), metaData$unit, sep = "")
  }

  return(captionSubset)
}

#' @title getAesStringMapping
#' @param dataMapping DataMapping class or subclass object
#' @description
#' Get the \code{dataMapping} elements and convert them into
#' character string usable by ggplot mapping function \code{aes_string}.
#' The conversion fixes any issue of special characters by wrapping the string by \code{``}
#' if not already used.
#'
#' \code{dataMapping} unmapped aesthetic elements (e.g. \code{color}) are associated to
#' the dummy aesthetic variable \code{"defaultAes"} which allows further modification of the plot aesthetics.
#' @return A list of mappings that can be used as is by \code{aes_string}.
#' @examples
#' \dontrun{
#' dataMapping <- XYGDataMapping$new(x = "Time [h]", y = "Concentration [mol/L]", color = "Dose")
#' mappingLabels <- getAesStringMapping(dataMapping)
#' }
getAesStringMapping <- function(dataMapping) {
  # Define list of mappings to check
  geomMappings <- c("x", "y", "ymin", "ymax", "lower", "middle", "upper")
  groupMappings <- names(LegendTypes)

  # Initialize Labels
  dataMappingLabels <- vector(mode = "list", length = length(geomMappings) + length(groupMappings))
  dataMappingLabels <- lapply(dataMappingLabels, function(x) {
    return("legendLabels")
  })

  names(dataMappingLabels) <- c(geomMappings, groupMappings)

  for (geomName in geomMappings) {
    if (!is.null(dataMapping[[geomName]])) {
      if (length(grep(pattern = "`", x = dataMapping[[geomName]])) == 0) {
        dataMappingLabels[[geomName]] <- paste0("`", dataMapping[[geomName]], "`")
      }
    }
  }

  for (groupName in groupMappings) {
    if (!is.null(dataMapping$groupMapping[[groupName]]$group)) {
      if (length(grep(pattern = "`", x = dataMapping$groupMapping[[groupName]]$label)) == 0) {
        dataMappingLabels[[groupName]] <- paste0("`", dataMapping$groupMapping[[groupName]]$label, "`")
      }
    }
  }
  return(dataMappingLabels)
}

#' @title smartMapping
#' @param data data.frame on which the smart mapping is used
#' @description
#' Check data size and variable names,
#' Get the mapping if variable names correspond to usual aesthetic or mapping names.
#' Else return NULL for the mapping property.
#'
#' If data has only one variable, it will be mapped as \code{x}.
#' If data has only 2 variables, they will be respectively mapped as \code{x} and \code{y}.
#'
#' Recognized names for the mapping are \code{x}, \code{y}, \code{ymin}, \code{ymax},
#' \code{lower}, \code{middle}, \code{upper}, \code{color}, \code{shape}, \code{linetype},
#' \code{size} and \code{fill}
#' @return A list of usual mappings that can be guessed from data
#' @examples
#' \dontrun{
#' # Get variable names x and y directly from data
#' data <- data.frame(x = c(1, 2, 3), y = c(6, 5, 4), z = c(7, 8, 9))
#' mapping <- smartMapping(data)
#'
#' # If data has aesthetic propoerties
#' data <- data.frame(x = c(1, 2, 3), y = c(6, 5, 4), color = c("blue", "red", "blue"))
#' mapping <- smartMapping(data)
#' }
smartMapping <- function(data) {
  # Initialize smart mapping with null values
  geomMappings <- c("x", "y", "ymin", "ymax", "lower", "middle", "upper")
  groupMappings <- names(LegendTypes)

  mapping <- vector(mode = "list", length = length(geomMappings) + length(groupMappings))
  names(mapping) <- c(geomMappings, groupMappings)

  if (is.null(data)) {
    return(mapping)
  }

  # Names of data.frame variables
  variableNames <- names(data)
  # If one column, set as y
  if (ncol(data) == 1) {
    mapping$y <- variableNames[1]
  }
  # If 2 columns, set as x, y
  if (ncol(data) == 2) {
    mapping$x <- variableNames[1]
    mapping$y <- variableNames[2]
  }

  # If data.frame variable name match a usual mapping,
  # set as mapping
  for (variableName in variableNames) {
    if (variableName %in% c(geomMappings, groupMappings)) {
      mapping[[variableName]] <- variableName
    }
  }
  return(mapping)
}

DefaultDataMappingValues <- list(
  pkRatio = list(
    pkRatio1 = 1,
    pkRatio2 = c(1.5, 1 / 1.5),
    pkRatio3 = c(2, 1 / 2)
  ),
  ddiRatio = list(
    ddiRatio1 = 1,
    ddiRatio2 = c(2, 1 / 2),
    guestLine = 1
  ),
  obsVsPred = 1,
  resVsPred = 0,
  tornado = 0,
  histogram = 0
)

#' @title DDIComparisonTypes
#' @description Options for comparison: residuals vs pred or obs vs pred
#' @export
DDIComparisonTypes <- enum(c("resVsPred", "obsVsPred"))

getAggregatedData <- function(data,
                              xParameterName,
                              yParameterName,
                              xParameterBreaks = NULL) {
  xParameterBreaks <- xParameterBreaks %||% tlfEnv$defaultAggregation$bins
  xParameterBins <- cut(data[, xParameterName], breaks = xParameterBreaks)

  xData <- stats::aggregate(
    x = data[, xParameterName],
    by = list(
      Bins = xParameterBins,
      Groups = data[, "legendLabels"]
    ),
    FUN = tlfEnv$defaultAggregation$functions$y
  )

  medianData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Groups = data[, "legendLabels"]
    ),
    FUN = tlfEnv$defaultAggregation$functions$y
  )

  lowPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Groups = data[, "legendLabels"]
    ),
    FUN = tlfEnv$defaultAggregation$functions$ymin
  )

  highPercData <- stats::aggregate(
    x = data[, yParameterName],
    by = list(
      Bins = xParameterBins,
      Groups = data[, "legendLabels"]
    ),
    FUN = tlfEnv$defaultAggregation$functions$ymax
  )

  aggregatedData <- cbind.data.frame(xData,
    y = medianData$x,
    ymin = lowPercData$x,
    ymax = highPercData$x
  )

  return(aggregatedData)
}
