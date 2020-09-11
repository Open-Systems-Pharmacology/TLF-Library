#' @title ExportConfiguration
#' @description R6 class defining the save configuration of a \code{ggplot} object
#' @export
ExportConfiguration <- R6::R6Class(
  "ExportConfiguration",
  public = list(
    #' @field format character defining the format of the file to be saved
    format = NULL,
    #' @field width numeric values defining the width in `units` of the plot dimensions after saving
    width = NULL,
    #' @field height numeric values defining the height in `units` of the plot dimensions after saving
    height = NULL,
    #' @field units character defining the unit of the saving dimension
    units = NULL,

    #' @description Create a new \code{ExportConfiguration} object
    #' @param format character defining the format of the file to be saved.
    #' @param width numeric values defining the width in `units` of the plot dimensions after saving
    #' @param height numeric values defining the height in `units` of the plot dimensions after saving
    #' @param units character defining the unit of the saving dimension
    #' @return A new \code{ExportConfiguration} object
    initialize = function(format = tlfEnv$defaultExportParameters$format,
                              width = tlfEnv$defaultExportParameters$width,
                              height = tlfEnv$defaultExportParameters$height,
                              units = tlfEnv$defaultExportParameters$units) {
      validateIsString(c(format, units))
      validateIsNumeric(c(width, height))

      self$format <- format
      self$height <- height
      self$width <- width
      self$units <- units
    },

    #' @description Print file export properties
    #' @return File export properties
    print = function() {
      exportProperties <- list(
        format = self$format,
        size = paste0(self$width, " ", self$units, " x ", self$height, " ", self$units)
      )
      return(exportProperties)
    }
  )
)
