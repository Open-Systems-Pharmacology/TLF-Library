#' @title saveConfiguration
#' @docType class
#' @description  Configuration to save a gglot
#' @field filename character
#' @field width numeric
#' @field height numeric
#' @field units character
#' @section Methods:
#' \describe{
#' \item{new()}{Initialize SaveConfiguration}
#' \item{print()}{Show SaveConfiguration properties}
#' \item{savePlot(plotObject)}{Save ggplot object into filename file}
#' }
#' @export
#' @format NULL
SaveConfiguration <- R6::R6Class(
  "SaveConfiguration",
  
  public = list(
    filename = NULL,
    height = NULL,
    width = NULL,
    units = NULL,
    
    initialize = function(filename = "tlf-plot.png",
                          height = 12,
                          width = 20,
                          units = "cm",
                          format = NULL,
                          path = NULL) {
      
      self$filename <-  paste0(c(paste0(c(path, filename), collapse = "/"),
                                 format), collapse = ".")
      
      self$height <- height
      self$width <- width
      self$units <- units
    },
    
    print = function() {
      cat("file name: ", self$filename, "\n", sep = " ")
      cat("plot size: ", self$width, "x", self$height, self$units, "\n", sep = " ")
      invisible(self)
    },
    
    savePlot = function(plotObject){
      ggplot2::ggsave(filename = self$filename,
                      plot = plotObject,
                      width = self$width,
                      height = self$height,
                      units = self$units)
      print(paste0("Plot saved as: ", self$filename))
    }
  )
)