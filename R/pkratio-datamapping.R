#' @title PKRatioDataMapping
#' @docType class
#' @description  Data Mapping for PKRatio
#' @export
#' 
#' 
Grouping <- R6::R6Class(
  "Grouping",
  public = list(
    groupingName = NULL,
    groupingDataFrame = NULL,
    groupingLegendTitle = NULL,
    initialize = function(groupingName = NULL, groupingDataFrame = NULL, groupingLegendTitle = NULL){
      
      self$groupingLegendTitle <- groupingLegendTitle
      groupingList <- self$getGroupingList(groupingName,groupingDataFrame)
      self$groupingName <- groupingList$selfGrouping
      self$groupingDataFrame <- groupingList$selfGroupingDataFrame
      
    },
    
    getGroupingList = function(grouping,groupingDataFrame){  #move out of public?
      if (!is.null(groupingDataFrame)){
        selfGroupingDataFrame <- groupingDataFrame
        selfGrouping<-tail(colnames(groupingDataFrame),1)  #the last column heading of the grouping dataframe will be a default legend title if no groupingLegendTitle is provided
      } else {
        selfGroupingDataFrame <- NULL
        selfGrouping <- grouping
      }
      groupingList <-list(selfGroupingDataFrame = selfGroupingDataFrame , selfGrouping = selfGrouping)
      return(groupingList)
    }
    
  )
)


PKRatioDataMapping <- R6::R6Class(
  "PKRatioDataMapping",
  inherit = XYDataMapping,
  public = list(
    colorGroupingObj = NULL,
    sizeGroupingObj = NULL,
    shapeGroupingObj = NULL,
    
    
    initialize = function(x = "Age",
                          y = "Ratio",
                          colorGrouping = NULL,
                          sizeGrouping = NULL,
                          shapeGrouping = NULL,
                          colorGroupingDataFrame = NULL,
                          sizeGroupingDataFrame = NULL,
                          shapeGroupingDataFrame = NULL,
                          colorLegendTitle = NULL,
                          sizeLegendTitle = NULL,
                          shapeLegendTitle = NULL) {
      
      self$colorGroupingObj <- Grouping$new(colorGrouping,colorGroupingDataFrame,colorLegendTitle)
      self$sizeGroupingObj  <- Grouping$new(sizeGrouping,sizeGroupingDataFrame,sizeLegendTitle)
      self$shapeGroupingObj <- Grouping$new(shapeGrouping,shapeGroupingDataFrame,shapeLegendTitle)
      super$initialize(x, y)
    },
    
    getGrouping = function(data, metaData) {
      
      markerTypeList = list( 
        list(grpObj = self$colorGroupingObj  , DefaultColName = "colorGrouping" ),
        list(grpObj = self$sizeGroupingObj   , DefaultColName = "sizeGrouping"  ),
        list(grpObj = self$shapeGroupingObj  , DefaultColName = "shapeGrouping" )
      ) 
      
      for ( markerType in markerTypeList){
        if (is.null( markerType$grpObj$groupingDataFrame )){ #case where no dataframe is supplied for grouping - use default captions
          data[[ markerType$DefaultColName ]] <- getDefaultCaptions(data = data, metaData = metaData, variableList = markerType$grpObj$groupingName )
        }
        else { #case where dataframe is supplied for grouping
          data <- addCaptionsColumn(data,markerType$grpObj$groupingDataFrame,newColName = markerType$DefaultColName )  
        }
      }
      
      return(data)
    }
  )
)




#' @title getDefaultCaptions
#' @param data input data.frame with variables to group by
#' @param metaData input data.frame with variables to group by
#' @param variableList groups as factor levels
#' @description
#' createGroupingVariable create a new column that groups the grouping variable
#' @return groupingVariable
#' @export
#'
getDefaultCaptions <- function(data, metaData, variableList = colnames(data)) {
  
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
      ), sep = "-"
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
    CaptionSubset <- paste(as.character(round(data)), metaData$Unit, sep = "")
  }
  
  return(CaptionSubset)
}




check_if_not_numeric <- function(vec,msg="Input must be numeric."){
  if (!is.numeric(vec)){
    stop(msg)
  }
} 


addCaptionsColumn <- function(df,dfinp,newColName = NULL){
  #df is the original dataframe
  #dfinp is a dataframe used to group df
  #newColName is a default legend title for this grouping
  sel_df_cols <- df[colnames( dfinp[ seq ( 1,ncol(dfinp)-1 )] )] #get columns from df corresponding to column headings in dfinp (excluding the last column heading in dfinp)
  df_caption_factors <- rep(0,nrow(df)) #vector that is to be populated with factor levels that determine caption
  df_inp_levels <- seq(1,nrow(dfinp)) #factor levels associated with each caption
  for (k in df_inp_levels){ #for each caption 
    logic_matrix <- matrix(rep(FALSE,nrow(df)*ncol(sel_df_cols)),nrow(df))
    for (n in seq(1,ncol(sel_df_cols))){#for each column
      col_head <- colnames(sel_df_cols[n]) #get column header
      if ( is.list(dfinp[[col_head]]) ) { #case where a list is supplied for binning
        check_if_not_numeric(sel_df_cols[[col_head]] , msg = "Dataframe column entries to be binned must be numeric.") #check that data points to be binned are numeric
        sapply( dfinp[[col_head]], check_if_not_numeric , msg = "Bin limits must be numeric") #check that all bin limits are numeric
        if (!all(sapply(dfinp[[col_head]],function(x) return( length(x) == 2 )))) { #check that all bin limits are of length 2.
          stop("Each element of bin limits list must be a vector of length 2.")
        }
        if (!all(sapply(dfinp[[col_head]],function(x) return(x[2]>x[1])))) { #check that all bin limits are in increasing order
          stop("Bin limits must be increasing.")
        }
        logic_matrix[,n]<-sapply( sel_df_cols[[col_head]], function(x) return( (x >= dfinp[[col_head]][[k]][1]) & (x <= dfinp[[col_head]][[k]][2])   )  )   
      } else { #case where there is no binning, only matching between caption dataframe entries and data column entries
        logic_matrix[,n]<-sapply( sel_df_cols[[col_head]], function(x) { return( x == dfinp[[col_head]][k] ) } )
      }
    }
    
    for (m in seq(1,nrow(df))){#for each row of data
      if (all(logic_matrix[m,])){ #if entire df row matches dfinp row
        df_caption_factors[m] <- df_inp_levels[k] #set factor level in df_caption_factors
      }
    }
   
    
  } 
  
  if (is.null(newColName))
  {
    newColName<-colnames(dfinp[ncol(dfinp)])
  }
  
  df[[newColName]]<- as.factor(rep(NA,nrow(df)))
  levels(df[[newColName]])<-dfinp[[ncol(dfinp)]]

  
      
  df[[newColName]][ df_caption_factors != 0 ] <- dfinp[[ncol(dfinp)]][ df_caption_factors[df_caption_factors!= 0 ] ]
  
 
  
  return(df)
}
