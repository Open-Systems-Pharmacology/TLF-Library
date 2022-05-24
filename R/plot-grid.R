#' Create a plot grid
#'
#' @param plotGridConfiguration  A `PlotGridConfiguration` object, which
#'   is an `R6` class object that defines properties of a plot grid (like number
#'   of rows, columns, labels, etc.).
#'
#' @description
#'
#' Create a plot grid using the `patchwork::wrap_plots()` function. The required
#' arguments are supplied through the `PlotGridConfiguration` object.
#'
#' @examples
#'
#' library(ggplot2)
#' library(tlf)
#'
#' # only `{tlf}` ---------------------
#'
#' # plots to be arranged in a grid
#' set.seed(123)
#' ls_plots <- list(
#'   plotHistogram(x = rnorm(100)),
#'   plotHistogram(x = rnorm(100, mean = 3)),
#'   plotHistogram(x = rnorm(100, mean = 10))
#' )
#'
#' # create an instance of plot configuration class
#' plotGridObj <- PlotGridConfiguration$new(plotList = ls_plots)
#'
#' # specify further customizations for the plot grid
#' plotGridObj$title <- "my combined plot"
#' plotGridObj$subtitle <- "something clever"
#' plotGridObj$caption <- "my sources"
#' plotGridObj$nColumns <- 2L
#' plotGridObj$tagLevels <- "A"
#' plotGridObj$tagPrefix <- "Plot ("
#' plotGridObj$tagSuffix <- ")"
#'
#' # plot the grid
#' plotGrid(plotGridObj)
#'
#' #  `{tlf}` and `{ggplot2}` ---------------------
#'
#' # `{tlf}` plot
#' set.seed(123)
#' p1 <- plotBoxWhisker(mtcars,
#'   dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
#' )
#'
#' # custom `{ggplot2}` plot
#' set.seed(123)
#' p2 <- ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point()
#'
#' # create an instance of plot configuration class
#' plotGridObj2 <- PlotGridConfiguration$new(list(p1, p2))
#'
#' # specify further customizations for the plot grid
#' plotGridObj2$nColumns <- 1L
#' plotGridObj2$tagLevels <- "i"
#'
#' # plot the grid
#' plotGrid(plotGridObj2)
#'
#' @references
#' For more, see: <https://patchwork.data-imaginist.com/articles/patchwork.html>
#'
#' @export
plotGrid <- function(plotGridConfiguration) {
  validateIsOfType(plotGridConfiguration, "PlotGridConfiguration")

  patchwork::wrap_plots(
    plotGridConfiguration$plotList,
    ncol = plotGridConfiguration$nColumns,
    nrow = plotGridConfiguration$nRows,
    byrow = plotGridConfiguration$byRow,
    widths = plotGridConfiguration$widths,
    heights = plotGridConfiguration$heights,
    guides = plotGridConfiguration$guides,
    design = plotGridConfiguration$design
  ) +
    patchwork::plot_annotation(
      title = plotGridConfiguration$title,
      subtitle = plotGridConfiguration$subtitle,
      caption = plotGridConfiguration$caption,
      tag_levels = plotGridConfiguration$tagLevels,
      tag_prefix = plotGridConfiguration$tagPrefix,
      tag_suffix = plotGridConfiguration$tagSuffix,
      tag_sep = plotGridConfiguration$tagSeparator,
      theme = plotGridConfiguration$theme
    )
}


#' @title Class for creating a plot grid
#'
#' @description
#'
#' R6 class defining the configuration for `{patchwork}` plot grid used to
#' create a grid of plots from `{tlf}`. It holds values for all relevant
#' plot properties.
#'
#' @field plotList A list containing `ggplot` objects.
#' @field title,subtitle,caption Text strings to use for the various plot
#' annotations, where plot refers to the grid of plots as a whole.
#' @field tagLevels A character vector defining the enumeration format to use
#' at each level. Possible values are `'a'` for lowercase letters, `'A'` for
#' uppercase letters, `'1'` for numbers, `'i'` for lowercase Roman numerals, and
#' `'I'` for uppercase Roman numerals. It can also be a list containing
#' character vectors defining arbitrary tag sequences. If any element in the
#' list is a scalar and one of `'a'`, `'A'`, `'1'`, `'i`, or `'I'`, this level
#' will be expanded to the expected sequence.
#' @field tagPrefix,tagSuffix Strings that should appear before or after the
#' tag.
#' @field tagSeparator A separator between different tag levels
#' @field theme A ggplot theme specification to use for the plot. Only elements
#' related to the titles as well as plot margin and background will be used.
#' @field nColumns,nRows The dimensions of the grid to create - if both are
#'   `NULL` it will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to
#'   set the dimensions
#' @field byRow Analogous to `byrow` in [matrix()][base::matrix]. If `FALSE` the
#' plots will be filled in in column-major order.
#' @field widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid.
#' @field guides A string specifying how guides should be treated in the layout.
#' `'collect'` will collect guides below to the given nesting level, removing
#' duplicates. `'keep'` will stop collection at this level and let guides be
#' placed alongside their plot. `auto` will allow guides to be collected if a
#' upper level tries, but place them alongside the plot if not. If you modify
#' default guide "position" with [theme(legend.position=...)][ggplot2::theme]
#' while also collecting guides you must apply that change to the overall
#' patchwork.
#' @field design Specification of the location of areas in the layout. Can
#'   either be specified as a text string or by concatenating calls to [area()]
#'   together. See the examples for further information on use.
#'
#' @examples
#'
#' library(tlf)
#'
#' # create a list of plots
#' ls_plots <- list(
#'   # first plot
#'   plotBoxWhisker(mtcars,
#'     dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
#'   ),
#'   # second plot
#'   plotBoxWhisker(ToothGrowth,
#'     dataMapping = BoxWhiskerDataMapping$new(x = "supp", y = "len")
#'   )
#' )
#'
#' plotGridObj <- PlotGridConfiguration$new(ls_plots)
#'
#' plotGridObj$title <- "my combined plot"
#' plotGridObj$subtitle <- "something clever"
#' plotGridObj$caption <- "something dumb"
#' plotGridObj$nColumns <- 2L
#' plotGridObj$nRows <- 1L
#' plotGridObj$byRow <- TRUE
#' plotGridObj$tagLevels <- "A"
#' plotGridObj$tagPrefix <- "Plot ("
#' plotGridObj$tagSuffix <- ")"
#'
#' # print the object to see its properties
#' plotGridObj
#'
#' @return A `PlotGridConfiguration` object.
#'
#' @export
PlotGridConfiguration <- R6::R6Class(
  "PlotGridConfiguration",
  inherit = ospsuite.utils::Printable,
  public = list(
    plotList = list(),
    title = NULL,
    subtitle = NULL,
    caption = NULL,
    tagLevels = NULL,
    tagPrefix = NULL,
    tagSuffix = NULL,
    tagSeparator = NULL,
    theme = NULL,
    nColumns = NULL,
    nRows = NULL,
    byRow = NULL,
    widths = NULL,
    heights = NULL,
    guides = NULL,
    design = NULL,

    #' @description Create an instance of `PlotGridConfiguration` class.
    #'
    #' @param plotList A list containing `ggplot` objects.
    #'
    #' @return A `PlotGridConfiguration` object.
    initialize = function(plotList = NULL) {
      self$plotList <- plotList
    },

    #' @description Add a plot object.
    #'
    #' @param plots A single or a list containing `ggplot` object(s).
    #'
    #' @examples
    #'
    #' library(ggplot2)
    #'
    #' myPlotGrid <- PlotGridConfiguration$new()
    #'
    #' # You can add a single ggplot object
    #' p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
    #' myPlotGrid$addPlots(p)
    #'
    #' # Or you can also pass a list
    #' myPlotGrid$addPlots(list("p1" = ggplot(), "p2" = ggplot()))
    #'
    #' # Since we added three plots, the `plotList` field should
    #' # now be a list of length `3`
    #' length(myPlotGrid$plotList)
    #'
    #' @return `PlotGridConfiguration` object with `$plotList` field updated to
    #'   store entered plots.
    addPlots = function(plots = NULL) {
      if (!is.null(plots)) {
        validateIsOfType(plots, "ggplot")

        # When only single instance of `ggplot` object is entered:
        # `addPlots = ggplot()`
        if (objectCount(plots) == 1L) {
          # This single object needs to be converted to a list *only if* it
          # already hasn't been entered in a list: `addPlots = list(ggplot())`.
          #
          # `ggplot` objects themselves are lists, but they will always have a
          # length greater than 1. That's how we can distinguish between
          # `ggplot` object itself as a list and the same object stored in a
          # list.
          if (is.list(plots) && length(plots) > 1L) {
            plots <- list(plots)
          }
        }

        self$plotList <- append(self$plotList, plots)
      }
    },

    #' @description
    #' Print the object to the console.
    print = function() {
      private$printClass()

      private$printLine("Plot grid annotations", addTab = TRUE)
      private$printLine("\tTitle", self$title, addTab = TRUE)
      private$printLine("\tSubtitle", self$subtitle, addTab = TRUE)
      private$printLine("\tCaption", self$caption, addTab = TRUE)

      private$printLine("Plot grid arrangement", addTab = TRUE)
      private$printLine("\tNumber of plots included", length(self$plotList), addTab = TRUE)
      private$printLine("\tNumber of columns in the grid", self$nColumns, addTab = TRUE)
      private$printLine("\tNumber of rows in the grid", self$nRows, addTab = TRUE)
      private$printLine("\tArranged in row-major order", self$byRow, addTab = TRUE)

      private$printLine("Individual plot tags", addTab = TRUE)
      private$printLine("\tTag level format", self$tagLevels, addTab = TRUE)
      private$printLine("\tTag level prefix", self$tagPrefix, addTab = TRUE)
      private$printLine("\tTag level suffix", self$tagSuffix, addTab = TRUE)
      private$printLine("\tTag level separator", self$tagSeparator, addTab = TRUE)
    }
  )
)
