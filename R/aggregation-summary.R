aggregationSummary <- R6::R6Class(
  "aggregationSummary",
  public = list(
    data = NULL,
    xColumnNames = NULL,
    groupingColumnNames = NULL,
    yColumnName = NULL,
    aggregationFunctionsVector = NULL,
    aggregationFunctionNames = NULL,
    dfHelper = NULL,
    initialize = function(data,
                          xColumnNames ,
                          groupingColumnNames = NULL,
                          yColumnName = NULL,
                          aggregationFunctionsVector = NULL,
                          aggregationFunctionNames = NULL) {
      self$data <- data
      self$xColumnNames <- xColumnNames
      self$groupingColumnNames <- groupingColumnNames
      self$yColumnName <- yColumnName
      self$aggregationFunctionsVector <- c(aggregationFunctionsVector)
      self$aggregationFunctionNames <- aggregationFunctionNames
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

      yValuesCol <- self$data[ self$yColumnName ] # Extract column of values from dataframe
      aggSummaries <- aggregate(yValuesCol, xGroupingCols, function(x) {
        res <- self$applyAggregationFunctions(x)
        return(res)
      })

      xAggCols <- aggSummaries[ xGroupingColNames ]
      summaryMatrix<-aggSummaries[[ self$yColumnName ]]
      self$dfHelper <- list()

      for (n in seq(1,length(self$aggregationFunctionNames))){
        dF<-xAggCols
        dF[self$aggregationFunctionNames[n]]<-summaryMatrix[,n]
        self$dfHelper[[self$aggregationFunctionNames[n]]]<-dF
      }

      #Splits the dataframe data into subsets defined by uniqe combinations of elements in the columns xColumnNames  and groupingColumnNames.
      #Applies functions defined in aggregationFunctionsVector to column yColumnName.
      #Returns a list of dataframes, one dataframe for each function listed in aggregationFunctionsVector.
      #Each dataframe in list element is named after the function's corresponding string in aggregationFunctionNames.
      #The summary statistic column name in each dataframe is the same as the name of the dataframe in the returned list.

      self$groupingColumnNames <- groupingColumnNames
      self$yColumnName <- yColumnName
      self$aggregationFunctionsVector <- c(aggregationFunctionsVector)

      # For each unique combination of entries in the rows of the grouping columns xGroupingCols, get corresponding values from yValuesCol and group these values into a vector.
      # Then for each such vector apply the functions in the vector aggregationFunctionsVector.  Output over all rows in xGroupingCols and functions in aggregationFunctionsVector forms a matrix
      # xGroupingCols and the output matrix are combined to form one dataframe called dfHelper.

    }
  )
)
