#' @title saveConfiguration
#' @description R6 class defining the save configuration of a \code{ggplot} object
#' @export
SaveConfiguration <- R6::R6Class(
  "SaveConfiguration",
  public = list(
    #' @field filename character defining the name of the file to be saved
    filename = NULL,
    #' @field width numeric values defining the width in `units` of the plot dimensions after saving
    width = NULL,
    #' @field height numeric values defining the height in `units` of the plot dimensions after saving
    height = NULL,
    #' @field units character defining the unit of the saving dimension
    units = NULL,

    #' @description Create a new \code{SaveConfiguration} object
    #' @param filename character defining the name of the file to be saved
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @param format character defining the name of the file to be saved.
    #' If `NULL` use "png" format.
    #' @param path character defining the path where to save the file.
    #' If `NULL` use current path.
    #' @return A new \code{SaveConfiguration} object
    initialize = function(filename = "tlf-plot.png",
                              width = 20,
                              height = 12,
                              units = "cm",
                              format = NULL,
                              path = NULL) {
      self$filename <- paste0(c(
        paste0(c(path, filename), collapse = "/"),
        format
      ), collapse = ".")

      self$height <- height
      self$width <- width
      self$units <- units
    },

    #' @description Print file save properties
    #' @return File save properties
    print = function() {
      saveProperties <- list(filename = self$filename,
                             format = paste0(self$width, " ", self$units, " x ", self$height, " ", self$units)
      )
      return(saveProperties)
    },

    #' @description Save \code{ggplot} object into a file
    #' @param plotObject a \code{ggplot} object
    #' @return NULL
    savePlot = function(plotObject) {
      ggplot2::ggsave(
        filename = self$filename,
        plot = plotObject,
        width = self$width,
        height = self$height,
        units = self$units
      )
      print(paste0("Plot saved as: ", self$filename))
    }
  )
)
