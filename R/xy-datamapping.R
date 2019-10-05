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
    errorMin = NULL,
    errorMax = NULL,
    color = NULL,
    size = NULL,
    shape = NULL,
    linetype = NULL,
    data = NULL,
    metaData = NULL,

    initialize = function(x,
                          y,
                          ###
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

