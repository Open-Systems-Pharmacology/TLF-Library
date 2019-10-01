#' @title Theme
#' @docType class
#' @description
#' Theme class sets of font properties for texts
#' @include utils.R
#' @export
Theme <- R6::R6Class(
  "Theme",
  public = list(
    titleFont = NULL,
    subtitleFont = NULL,
    xlabelFont = NULL,
    ylabelFont = NULL,
    watermarkFont = NULL,

    aesProperties = NULL,

    pkRatioLinesProperties = NULL,

    initialize = function(labelColors = labelColorPalette$default,
                              labelBaseSize = 12,
                              aesProperties = aesProperties$default,
                              pkRatioLinesProperties = pkRatioLinesPropertiesDefault) {

      # Set font properties of labels
      self$titleFont <- Font$new(color = labelColors$title, size = labelBaseSize * 2, fontFace = "bold")
      self$subtitleFont <- Font$new(color = labelColors$subtitle, size = labelBaseSize * 1.5, fontFace = "italic")
      self$xlabelFont <- Font$new(color = labelColors$xlabel, size = labelBaseSize)
      self$ylabelFont <- Font$new(color = labelColors$ylabel, size = labelBaseSize)
      self$watermarkFont <- Font$new(color = labelColors$watermark, size = labelBaseSize)

      # Set the color, shape, size maps
      self$aesProperties <- aesProperties

      # Set the properties of specific plots
      self$pkRatioLinesProperties <- pkRatioLinesProperties
    },

    setPlotConfiguration = function(PlotConfiguration) {
      PlotConfiguration$title$font <- self$title
      PlotConfiguration$subtitle$font <- self$subtitle
      PlotConfiguration$xlabel$font <- self$xlabel
      PlotConfiguration$ylabel$font <- self$ylabel
      PlotConfiguration$watermark$font <- self$watermark
    }
  )
)

## -------------------------------------------------
# Define color palettes for plot labels
labelColorPalette <- enum(c("default", "tlf", "bw"))
labelColorPalette$default <- list(title = "darkblue", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "blue")
labelColorPalette$tlf <- list(title = "darkblue", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "green")
labelColorPalette$bw <- list(title = "black", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "grey50")


## -------------------------------------------------
# Define default colorMaps for plots
aesProperties <- enum(c("default", "tlf", "bw"))
aesProperties$default <- list(
  color = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  size = seq(1, 10),
  shape = seq(1, 10), # c("circle", "square", "diamond", "triangle"),
  linetype = c("solid", "dashed", "dotted")
)

aesProperties$tlf <- list(
  color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  size = seq(1, 10),
  shape = c("circle", "square", "diamond", "triangle"),
  linetype = c("solid", "dashed", "dotted")
)

aesProperties$bw <- list(
  color = c("#000000", "#999999", "#555555"),
  size = seq(1, 10),
  shape = c("circle", "square", "diamond", "triangle"),
  linetype = c("solid", "dashed", "dotted")
)

aesProperties$big <- aesProperties$default
aesProperties$big$size <- seq(4, 20, 2)

## -------------------------------------------------
# Define PK Ratio Lines Properties
pkRatioLinesPropertiesDefault <- data.frame(
  value = c(1, 1.5, 1 / 1.5, 2, 1 / 2),
  linetype = c("solid", "dashed", "dashed", "dashed", "dashed"),
  color = c("black", "blue", "blue", "red", "red"),
  size = c(2, 1, 1, 1, 1)
)

# Otherwise linetype and color are assumed as levels
pkRatioLinesPropertiesDefault$linetype <- as.character(pkRatioLinesPropertiesDefault$linetype)
pkRatioLinesPropertiesDefault$color <- as.character(pkRatioLinesPropertiesDefault$color)

# Black and White properties
pkRatioLinesPropertiesBW <- pkRatioLinesPropertiesDefault
pkRatioLinesPropertiesBW$color <- as.character(c("#000000", "#555555", "#555555", "#999999", "#999999"))
pkRatioLinesPropertiesBW$linetype <- as.character(c("solid", "dashed", "dashed", "dotted", "dotted"))

# Big theme properties
pkRatioLinesPropertiesBig <- pkRatioLinesPropertiesDefault
pkRatioLinesPropertiesBig$size <- pkRatioLinesPropertiesDefault$size * 2

## -------------------------------------------------
# Definition of default theme
defaultTheme <- Theme$new(
  labelColors = labelColorPalette$default,
  labelBaseSize = 12,
  aesProperties = aesProperties$default
)

# Definition of tlf theme
tlfTheme <- Theme$new(
  labelColors = labelColorPalette$tlf,
  labelBaseSize = 12,
  aesProperties = aesProperties$tlf
)

# Definition of black and white theme
bwTheme <- Theme$new(
  labelColors = labelColorPalette$bw,
  labelBaseSize = 12,
  aesProperties = aesProperties$bw,
  pkRatioLinesProperties = pkRatioLinesPropertiesBW
)

# Definition of black and white theme
bigTheme <- Theme$new(
  labelColors = labelColorPalette$default,
  labelBaseSize = 16,
  aesProperties = aesProperties$default,
  pkRatioLinesProperties = pkRatioLinesPropertiesBig
)


## -------------------------------------------------
# Util functions
useTheme <- function(theme) {
  tlfEnv$currentTheme <- theme
}
