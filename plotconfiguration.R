#' @title PlotConfiguration
#' @docType class
#' @description  Generic Plot Configuration
#' @export
PlotConfiguration <- R6::R6Class(
  "PlotConfiguration",
  ## ----------------------------------
  ## List of plotConfiguration Variables
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    xlim = c(-Inf, Inf),
    ylim = c(-Inf, Inf),
    watermark = NULL,
    legendCaption = NULL,
    legendPosition = NULL,
    filename = NULL,
    
    ## ----------------------------------------------
    ## Initializing function to be called with $new()
    initialize = function(title = asTitle(""),
                          subtitle = asLabel(paste("Date:", format(Sys.Date(), "%y-%m-%d"))),
                          xlabel = asLabel(""),
                          ylabel = asLabel(""),
                          xlim = c(-Inf, Inf),
                          ylim = c(-Inf, Inf),
                          watermark = asLabel("TLF-Watermark"),
                          filename = "TestPlot.png",
                          legendCaption = asLabel(""),
                          legendPosition = list("Location" = "outside", "X.Location" = "right", "Y.Location" = NULL)) {
      self$title <- asTitle(title)
      self$subtitle <- asLabel(subtitle)
      self$xlabel <- asLabel(xlabel)
      self$ylabel <- asLabel(ylabel)
      
      self$watermark <- asLabel(watermark)
      self$filename <- filename
      
      self$legendCaption <- asLabel(legendCaption)
      self$legendPosition <- legendPosition
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
    
    # ---------------------------------------------------------------
    # Define Labels: plotConfiguration function that uses data mapping
    # to set labels and legends
    defineLabels = function(plotHandle, dataMapping) { 
      # Titles and axes labels
      plotHandle <- plotHandle + ggplot2::labs(
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
        legendFont = self$legendCaption$font
      )

      # Legend labels
      plotHandle <- setLegendPosition(
        plotHandle,
        Location = self$legendPosition$Location,
        X.Location = self$legendPosition$X.Location,
        Y.Location = self$legendPosition$Y.Location
      )

      # Redefine label of groups in legend
      if (is.null(dataMapping$colorGroupingObj$groupingName)) { 
        plotHandle <- plotHandle + ggplot2::guides(color = "none") 
      } else {  
        colorLegendTitle = dataMapping$colorGroupingObj$groupingLegendTitle  %||% paste(dataMapping$colorGroupingObj$groupingName, collapse = "-")
        plotHandle <- plotHandle +
          ggplot2::guides(color = guide_legend(colorLegendTitle))
      }
      if (is.null(dataMapping$sizeGroupingObj$groupingName)) { 
        plotHandle <- plotHandle + ggplot2::guides(size = "none") 
      } else {  
        sizeLegendTitle = dataMapping$sizeGroupingObj$groupingLegendTitle  %||% paste(dataMapping$sizeGroupingObj$groupingName, collapse = "-")
        plotHandle <- plotHandle +
          ggplot2::guides(size = guide_legend(sizeLegendTitle))
      }
      if (is.null(dataMapping$shapeGroupingObj$groupingName)) { 
        plotHandle <- plotHandle + ggplot2::guides(shape = "none") 
      } else {  
        shapeLegendTitle = dataMapping$shapeGroupingObj$groupingLegendTitle  %||% paste(dataMapping$shapeGroupingObj$groupingName, collapse = "-")
        plotHandle <- plotHandle +
          ggplot2::guides(shape = guide_legend(shapeLegendTitle))
      }
      return(plotHandle)
    },
    
    ## ----------------------------------------------------------
    ## Legend Captions: plotConfiguration function to relabel and re-order legendcaption
    relabelGrouping = function(data, legendType, which, newCaption) {
      if ("color" %in% legendType) {
        levels(data$colorGroupingObj$groupingName)[which] <- newCaption
      }
      if ("size" %in% legendType) {
        levels(data$sizeGroupingObj$groupingName)[which] <- newCaption
      }
      if ("shape" %in% legendType) {
        levels(data$shapeGroupingObj$groupingName)[which] <- newCaption
      }
    },
    
    
    ## ----------------------------------------------------------
    ## Define Labels: plotConfiguration function to first define Watermark
    
    setWatermark = function(plotHandle) {
      plotHandle <- setWatermark(plotHandle = plotHandle, label = self$watermark)
      return(plotHandle)
    },
    
    savePlot = function(plotHandle) {
      ggplot2::ggsave(filename = self$filename, plotHandle, width = 20, height = 12, units = "cm")
    },
    ## ----------------------------------------------------------
    ## Define Some Default Themes using enum ?
    # TO BE DISCUSSED
    
    setTheme = function(theme = "default") {
      if (theme %in% "default") {
        self$title$setFontProperties(color = "firebrick4", size = 30, fontFace = "bold")
        self$subtitle$setFontProperties(color = "darkslateblue", size = 26, fontFace = "italic")
        self$xlabel$setFontProperties(color = "deepskyblue4", size = 20)
        self$ylabel$setFontProperties(color = "deepskyblue4", size = 20)
        self$watermark$setFontProperties(color = "goldenrod3", size = 14)
      }
      
      if (theme %in% "BW") {
        self$title$setFontProperties(color = "gray10")
        self$subtitle$setFontProperties(color = "gray20")
        self$xlabel$setFontProperties(color = "gray30")
        self$ylabel$setFontProperties(color = "gray30")
        self$watermark$setFontProperties(color = "gray40")
      }
    }
  )
)
