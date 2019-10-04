#' @title XYDataMapping
#' @docType class
#' @description  Abstract class for X Y Mapping
#' @export
#'

XYDataMapping <- R6::R6Class(
  "XYDataMapping",
  public = list(
    x = NULL,
    y = NULL,
    error = NULL,
    errorMin = NULL,
    errorMax = NULL,
    color = NULL,
    size = NULL,
    shape = NULL,
    linetype = NULL,
    data = NULL,


    initialize = function(x, y,
                          ###
                          error = NULL,
                          errorMin = NULL,
                          errorMax = NULL,
                          ###
                          color = NULL, #string or vector of strings that are dataframe colnames to group by color
                          size = NULL, #string or vector of strings that are dataframe colnames to group by size
                          shape = NULL, #string or vector of strings that are dataframe colnames to group by shape
                          linetype = NULL, #string or vector of strings that are dataframe colnames to group by linetype
                          ###
                          colorGroupingDataFrame = NULL,
                          sizeGroupingDataFrame = NULL,
                          shapeGroupingDataFrame = NULL,
                          ###
                          linetypeGroupingDataFrame = NULL,
                          colorLegendTitle = NULL,
                          sizeLegendTitle = NULL,
                          shapeLegendTitle = NULL,
                          linetypeLegendTitle = NULL,
                          ###
                          data = NULL,
                          metaData = NULL) {

      self$x <-x
      self$y <-y

      self$error <- error
      self$errorMin <- errorMin
      self$errorMax <- errorMax

      self$color    <- Grouping$new(color,colorGroupingDataFrame,colorLegendTitle)
      self$size     <- Grouping$new(size,sizeGroupingDataFrame,sizeLegendTitle)
      self$shape    <- Grouping$new(shape,shapeGroupingDataFrame,shapeLegendTitle)
      self$linetype <- Grouping$new(linetype,linetypeGroupingDataFrame,linetypeLegendTitle)

      if (!is.null(data)) {
        self$data <- self$getMapData(data, metaData)
      }
    },

    getMapData = function(data, metaData = NULL) {


      #Takes dataframe as input.
      #Extracts x and y from dataframe.
      #Sets the object's data property to be a dataframe with an x column , y column, color column, shape column...
      x <- data[, self$x]
      y <- data[, self$y]


      if ( (!is.null( self$errorMin )) & (!is.null( self$errorMax )) ){

        errorMin <- data[, self$errorMin]
        errorMax <- data[, self$errorMax]

      } else if (!is.null(self$error)){

        error <- data[, self$error]
        errorMin <- y - error
        errorMax <- y + error

      } else {

        errorMin <- rep(0,length(x))
        errorMax <- rep(0,length(x))

      }

      plotProperties <- list()

      grpObjList = list(list(obj = self$color    , name = "color"    ),
                        list(obj = self$size     , name = "size"     ),
                        list(obj = self$shape    , name = "shape"    ),
                        list(obj = self$linetype , name = "linetype" )
                        )

      for ( grpObj in grpObjList ){
        if (is.null( grpObj$obj$groupingDataFrame )){ #case where no dataframe is supplied for grouping - use default captions
          plotProperties[[grpObj$name]] <- getDefaultCaptions( data = data, metaData = metaData, variableList = grpObj$obj$groupingName )
        }
        else { #case where dataframe is supplied for grouping
          plotProperties[[grpObj$name]] <- getCustomCaptions( data , grpObj$obj$groupingDataFrame )
        }
      }

      self$data <- data.frame(
        "x" = x, "y" = y,
        "errorMin" = errorMin,
        "errorMax" = errorMax,
        "color" = plotProperties$color,
        "shape" = plotProperties$shape,
        "size"  = plotProperties$size,
        "linetype" = plotProperties$linetype

      )




      return(self$data)
    }
  )
)



checkIfNotNumeric <- function(vec,msg="Input must be numeric."){
  if (!is.numeric(vec)){
    stop(msg)
  }
}


binNumericCol <- function(binLimits,currentDataCol ){#function to iterate through a numeric column and find elements that fall within bin limits.  Returns a logical column of the same size as the input data column.
  checkIfNotNumeric(currentDataCol , msg = "Dataframe column entries to be binned must be numeric.") #check that dataDf points to be binned are numeric
  sapply( binLimits, checkIfNotNumeric , msg = "Bin limits must be numeric") #check that all bin limits are numeric
  if (!(length(binLimits ) == 2) ) { #check that bin limits are of length 2.
    stop("Each element of bin limits list must be a vector of length 2.")
  }
  if (!(binLimits[2]>binLimits[1]) ) { #check that bin limits are in increasing order
    stop("Bin limits must be increasing.")
  }
  dataPointsInGrp <- sapply( currentDataCol , function(x) return( (x >= binLimits[1]) & (x <= binLimits[2])   )  )
  return( dataPointsInGrp )
}



findColumnEntriesInGroups <- function(dataDf,groupingDfRow){
  groupingDataColumns <- dataDf[ colnames( groupingDfRow ) ] #get columns from dataDf corresponding to column headings in groupingDf (excluding the last column heading in groupingDf)
  logicMatrix <- matrix(rep(FALSE,nrow(dataDf)*ncol(groupingDfRow)),nrow(dataDf))
  for (n in seq(1,ncol(groupingDfRow)) ){#for each column of the groupingDf
    if ( is.list( groupingDfRow[[n]] ) ) { #case where a list is supplied for binning
      logicMatrix[,n]  <- binNumericCol( groupingDfRow[[n]][[1]] ,groupingDataColumns[[n]]) #check if each column entry lies within bin limits.  If yes, return TRUE for that entry, else FALSE.
    } else { #case where there is no binning, only matching between caption dataframe entries and dataDf column entries
      logicMatrix[,n] <-sapply( groupingDataColumns[[n]], function(x) { return( x == groupingDfRow[[n]] ) } ) #check if each column entry matches groupingDf element.  If yes, return TRUE for that entry, else FALSE.
    }
  }
  return(logicMatrix)
}



getCustomCaptions <- function(dataDf,groupingDf,newColName = NULL){
  #dataDf is the original dataframe
  #groupingDf is a dataframe used to group dataDf
  #newColName is a default legend title for this grouping
  stopifnot(ncol(groupingDf)>1)
  vecIndxGroupingDfRows <- seq(1,nrow(groupingDf)) #vector of factor levels associated with each caption
  groupingFactorColumn <- rep(0,nrow(dataDf)) #vector that is to be populated with factor levels that determine caption
  for (k in vecIndxGroupingDfRows){ #for each caption
    logicMatrix <- findColumnEntriesInGroups(dataDf, groupingDf[k, -ncol(groupingDf) , drop=FALSE] ) #call function to test if each column entry falls within the grouping
    for (m in seq(1,nrow(dataDf))){#for each row of dataDf
      if (all(logicMatrix[m,])){ #if entire dataDf row matches groupingDf row
        groupingFactorColumn[m] <- vecIndxGroupingDfRows[k] #set factor level in groupingFactorColumn to k
      }
    }
  }
  if (is.null(newColName)) #rename new grouping column if a name newColName is supplied
  {
    newColName<-colnames(groupingDf[ncol(groupingDf)])
  }
  newCol  <- as.factor(rep(NA,nrow(dataDf))) #add a factor column of NA to the dataframe dataDF.  For rows falling within the grouping, the NA will be modified to that grouping's caption.
  levels(newCol)<-groupingDf[[ncol(groupingDf)]] #Set factor levels of new column to be the captions of the grouping dataframe
  newCol[ groupingFactorColumn != 0 ] <- groupingDf[[ncol(groupingDf)]][ groupingFactorColumn[groupingFactorColumn!= 0 ] ] #Add the appropriate caption to each column entry that correspondinds to a dataDf row that falls within groupings in groupingDf
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

  # Paste the consecutively the variable names
  for (variable in tail(variableList, -1)) {
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
  CaptionSubset <- as.character(data)

  # If numeric create a character as rounded numeric + unit from metadata
  if ("numeric" %in% class(data)) {
    CaptionSubset <- paste(as.character(round(data)), metaData$unit, sep = "")
  }

  return(CaptionSubset)
}
