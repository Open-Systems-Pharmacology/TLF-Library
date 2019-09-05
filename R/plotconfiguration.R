#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  ## ----------------------------------
  ## List of plotConfiguration Variables
  public = list(
    title = asLabel(""),
    subtitle = asLabel(""),
    xlabel = asLabel(""),
    ylabel = asLabel(""),
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    watermark = asLabel(""),
    legend = list("Location" = "outside", "X.Location" = "right", "Y.Location" = "top", "font" = NULL),
    theme = NULL,


    ## ----------------------------------------------
    ## Initializing function to be called with $new()
    initialize = function(title = asLabel(""),
                          subtitle = asLabel(paste("Date:", format(Sys.Date(), "%y-%m-%d"))),
                          xlabel = asLabel(""),
                          ylabel = asLabel(""),
                          xlim = c(-Inf, Inf),
                          ylim = c(-Inf, Inf),
                          watermark = asLabel("TLF-Watermark"),
                          legend = list("Location" = "outside", "X.Location" = "right", "Y.Location" = NULL),
                          theme = NULL) {
      self$title <- title
      self$subtitle <- subtitle
      self$xlabel <- xlabel
      self$ylabel <- ylabel

      self$watermark <- watermark

      self$legend <- legend
    },

    ## ---------------------------------------------------------------
    ## Printing function, only show main elements of plotConfiguration

    print = function() {
      cat("  Title: ", self$title$text, "\n", sep = "\t")
      cat("  Subtitle: ", self$subtitle$text, "\n", sep = "\t")
      cat("  Xlabel:  ", self$xlabel$text, "\n", sep = "\t")
      cat("  Ylabel:  ", self$ylabel$text, "\n", sep = "\t")
      invisible(self)
    },

    ## ---------------------------------------------------------------
    ## Define Labels: plotConfiguration function that uses data mapping
    ## to set labels and legends
    defineLabels = function(plotHandle, dataMapping) {

      # Titles and axes labels
      plotHandle <- plotHandle + labs(
        title = self$title$text,
        subtitle = self$subtitle$text,
        x = self$xlabel$text,
        y = self$ylabel$text
      )

      plotHandle <- setFontProperties(
        plotHandle,
        titleFont = self$title$font,
        subtitleFont = self$subtitle$font,
        xAxisFont = self$xlabel$font,
        yAxisFont = self$ylabel$font,
        legendFont = self$legend$font
      )

      # Legend labels
      plotHandle <- setLegendPosition(
        plotHandle,
        Location = self$legend$Location,
        X.Location = self$legend$X.Location,
        Y.Location = self$legend$Y.Location
      )

      # Redefine label of groups in legend
      if (is.null(dataMapping$colorGrouping)) {
        plotHandle <- plotHandle + guides(color = "none")
      } else {
        plotHandle <- plotHandle + guides(color = guide_legend(dataMapping$colorGrouping))
      }
      if (is.null(dataMapping$sizeGrouping)) {
        plotHandle <- plotHandle + guides(size = "none")
      } else {
        plotHandle <- plotHandle + guides(size = guide_legend(dataMapping$sizeGrouping))
      }
      if (is.null(dataMapping$shapeGrouping)) {
        plotHandle <- plotHandle + guides(shape = "none")
      } else {
        plotHandle <- plotHandle + guides(shape = guide_legend(dataMapping$shapeGrouping))
      }

      return(plotHandle)
    },
    ## ----------------------------------------------------------
    ## Define Labels: plotConfiguration function to first define Watermark

    setWatermark = function(plotHandle) {
      plotHandle <- setWatermark(plotHandle = plotHandle, label = self$watermark)
      return(plotHandle)
    },
    ## ----------------------------------------------------------
    ## Define Some Themes using enum ?

    setTLFTheme = function() {
      self$title$setFontProperties(color = "firebrick4", size = 30, fontface = "bold")
      self$subtitle$setFontProperties(color = "darkslateblue", size = 26, fontface = "italic")
      self$xlabel$setFontProperties(color = "deepskyblue4", size = 20)
      self$ylabel$setFontProperties(color = "deepskyblue4", size = 20)

      self$watermark$setFontProperties(color = "goldenrod3", size = 14)
    },

    setBWFont = function() {
      self$title$setFontProperties(color = "gray10")
      self$subtitle$setFontProperties(color = "gray20")
      self$xlabel$setFontProperties(color = "gray30")
      self$ylabel$setFontProperties(color = "gray30")
      self$watermark$setFontProperties(color = "gray40")
    }
  )
)