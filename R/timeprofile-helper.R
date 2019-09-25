timeProfileHelper <- R6::R6Class(
  "timeProfileHelper",
  public = list(
    data = NULL,
    timeColumnName = NULL,
    groupingColumnNames = NULL,
    valuesColumnNames = NULL,
    aggregationFunctionsVector = NULL,
    aggregationFunctionNames = NULL,
    dfHelper = NULL,
    initialize = function(data = NULL,
                          timeColumnName = NULL,
                          groupingColumnNames = NULL,
                          valuesColumnNames = NULL,
                          aggregationFunctionsVector = NULL,
                          aggregationFunctionNames = NULL) {
      
      
      self$data = data
      self$timeColumnName = timeColumnName
      self$groupingColumnNames = groupingColumnNames
      self$valuesColumnNames = valuesColumnNames
      self$aggregationFunctionsVector = aggregationFunctionsVector
      self$aggregationFunctionNames = aggregationFunctionNames
      self$generateAggregatedValues()
    },
    
    applyAggregationFunctions = function(x){
      #input the vector of aggregated x into each aggregation function y and return the results in a vection res
      res<-sapply(self$aggregationFunctionsVector , function(y) { y(x) }  )  
      return(res)
    },
    
    generateAggregatedValues = function(){

      xGroupingColNames<-c(self$timeColumnName,self$groupingColumnNames) #Get names of grouping columns and groups then into a vector xGroupingColNames
      xGroupingCols <- lapply(xGroupingColNames,function(x){ self$data[[x]] })#Extract grouping columns from dataframe and group them into a list called xGroupingCols
      yValuesCol <- self$data[ self$valuesColumnNames ] #Extract column of values from dataframe
      dfHelper <- aggregate(yValuesCol,xGroupingCols, function(x) { res<-self$applyAggregationFunctions(x) ; return(res) }  ) 
      print(xGroupingCols)
      #For each unique combination of entries in the rows of the grouping columns xGroupingCols, get corresponding values from yValuesCol and group these values into a vector.
      #Then for each such vector apply the functions in the vector aggregationFunctionsVector.  Output over all rows in xGroupingCols and functions in aggregationFunctionsVector forms a matrix
      #xGroupingCols and the output matrix are combined to form one dataframe called dfHelper.
      
      colnames(dfHelper)[seq(1,length(xGroupingColNames))]<-xGroupingColNames #Rename columns corresponding to xGroupingCols in dfHelper
      
      dfHelper <- cbind(   dfHelper[,seq(1,ncol(dfHelper)-1)]       , as.data.frame(dfHelper$Simulated)   ) #Split last (matrix) column of dfHelper into a series of columns, each corresponding to a different function in aggregationFunctionsVector 
      resultCols<-seq(length(xGroupingColNames)+1,length(xGroupingColNames)+length(self$aggregationFunctionsVector)) #Rename the split columns according to the names of the functions supplied in aggregationFunctionNames
      colnames(dfHelper)[resultCols]<-self$aggregationFunctionNames 
      self$dfHelper <- dfHelper
    }
    
    
  )
)

# #Example:
# individualValues<-data.frame("Time" = c(0,1, 2, 0, 1, 2, 0, 1, 2, 0, 1, 2),
#            "IndividualID" = c(1, 1,1, 2, 2, 2, 1 ,1 ,1 ,2, 2, 2),
# "Population" = c("Asian","Asian","Asian","Asian","Asian","Asian","Asian","Asian ","Asian ","Asian ","Asian","Asian"),
# "Gender"  = c("M","F","M","F","M","M","M","F","M","F","M","M"),
# "Organ"= c("Venous Blood","Venous Blood","Venous Blood","Venous Blood","Venous Blood","Venous Blood","Brain","Brain","Brain","Brain","Brain","Brain"),
# "Compartment" = c("Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma","Plasma"),
# "Simulated" = c(2.0,3.0,12.0,9.,3.1,3.3,2.0,3.0,12.0,9.0,3.1,3.3),
# "Observed" =c(NA,2,10,NA,NA,NA,NA,NA,NA,NA,3,4),
# "Error"  = c(NA,0.1,0.2,NA,NA,NA,NA,NA,NA,NA,0.1,0.2))
# 
# 
# TC <- "Time"
# ACs <- "IndividualID"
# GCs <- c("Organ", "Compartment")
# VCs <- "Simulated"
# aggregationFunctionsVector<- c(min,max,mean,sd)
# aggregationFunctionNames <- c("Simulated Min","Simulated Max","Simulated Mean","Simulated SD")
# 
# newHelper <- timeProfileHelper$new(data = individualValues,
#                                    timeColumnName = TC,
#                                    groupingColumnNames = GCs,
#                                    valuesColumnNames = VCs,
#                                    aggregationFunctionsVector = aggregationFunctionsVector,
#                                    aggregationFunctionNames = aggregationFunctionNames)
# 
# print(newHelper$dfHelper)



