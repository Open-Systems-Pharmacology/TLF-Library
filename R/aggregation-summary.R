#' @title AggregationSummary
#' @docType class
#' @description  #Splits the dataframe data into subsets defined by unique combinations of elements in the columns xColumnNames and groupingColumnNames.
#Applies functions defined in aggregationFunctionsVector to column yColumnNames.
#Returns a list of dataframes, one dataframe for each function listed in aggregationFunctionsVector.
#Each dataframe in list element is named after the function's corresponding string in aggregationFunctionNames.
#The summary statistic column name in each dataframe is the same as the name of the dataframe in the returned list.
#' @export


AggregationSummary <- R6::R6Class(
  "AggregationSummary",
  public = list(
    data = NULL,
    metaData = NULL,
    xColumnNames = NULL,
    groupingColumnNames = NULL,
    yColumnNames = NULL,
    aggregationFunctionsVector = NULL,
    aggregationFunctionNames = NULL,
    aggregationUnitsVector = NULL,
    aggregationDimensionsVector = NULL,
    dfHelper = NULL,
    metaDataHelper = NULL,
    initialize = function(data,
                          metaData = NULL,
                          xColumnNames = NULL,
                          groupingColumnNames = NULL,
                          yColumnNames = NULL,
                          aggregationFunctionsVector = NULL,
                          aggregationFunctionNames = NULL,
                          aggregationUnitsVector = NULL,
                          aggregationDimensionsVector = NULL) {
      self$data <- data
      self$metaData <- metaData
      self$xColumnNames <- xColumnNames
      self$groupingColumnNames <- groupingColumnNames
      self$yColumnNames <- yColumnNames
      self$aggregationFunctionsVector <- c(aggregationFunctionsVector)
      self$aggregationFunctionNames <- aggregationFunctionNames
      self$aggregationUnitsVector = aggregationUnitsVector
      self$aggregationDimensionsVector = aggregationDimensionsVector
      self$generateAggregatedValues()
    },

    applyAggregationFunctions = function(x) {
      # input the vector of aggregated x into each aggregation function y and return the results in a vector res
      res <- sapply(self$aggregationFunctionsVector, function(y) {
        y(x)
      })
      return(res)
    },

    generateAggregatedValues = function() {

      xGroupingColNames <- c(self$xColumnNames, self$groupingColumnNames) # Get names of grouping columns and groups then into a vector xGroupingColNames

      xGroupingCols <- self$data[xGroupingColNames] # Extract grouping columns from dataframe and group them into a list called xGroupingCols

      yValuesCol <- self$data[ self$yColumnNames ] # Extract column of values from dataframe
      aggSummaries <- aggregate(yValuesCol, xGroupingCols, function(x) {
        res <- self$applyAggregationFunctions(x)
        return(res)
      })

      summaryMatrix<-matrix(aggSummaries[[ self$yColumnNames ]],ncol=length(self$aggregationFunctionsVector))



      self$dfHelper <- aggSummaries[ xGroupingColNames ]
      self$metaDataHelper <- self$metaData[xGroupingColNames]

      for (n in seq(1,length(self$aggregationFunctionNames))){


        dF<-data.frame(summaryMatrix[,n])
        colnames(dF)[1]<-self$aggregationFunctionNames[n]
        self$dfHelper <-cbind(self$dfHelper,dF)

        self$metaDataHelper [[self$aggregationFunctionNames[n]]]<-list(unit = self$aggregationUnitsVector[n] , dimension = self$aggregationDimensionsVector[n] )



      }


    }
  )
)
