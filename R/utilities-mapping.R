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
#' @param data input data.frame with variables to group by
#' @param metaData input data.frame with variables to group by
#' @param variableList groups as factor levels
#' @param sep characters separating variables in caption
#' @description
#' getDefaultCaptions create a new column that groups the grouping variable
#' @return groupingVariable
#' @export
#'
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

# Function Converting DataMappings to aes_string mapping
# Accounts for special character issues by using ``
getAesStringMapping <- function(dataMapping) {
  # Define list of mappings to check
  geomMappings <- c("x", "y", "yMin", "yMax")
  groupMappings <- names(LegendTypes)

  # Initialize Labels
  dataMappingLabels <- vector(mode = "list", length = length(geomMappings) + length(groupMappings))
  dataMappingLabels <- lapply(dataMappingLabels, function(x) {
    return("defaultAes")
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
    if (!is.null(dataMapping$groupMapping[[groupName]])) {
      if (length(grep(pattern = "`", x = dataMapping$groupMapping[[groupName]]$label)) == 0) {
        dataMappingLabels[[groupName]] <- paste0("`", dataMapping$groupMapping[[groupName]]$label, "`")
      }
    }
  }
  return(dataMappingLabels)
}
