# Theme Properties and theme object definitions
#
## -------------------------------------------------
# Define color palettes for plot labels
themeLabelColors <- enum(c("default", "tlf", "bw"))
themeLabelColors$default <- list(
  title = "darkblue",
  subtitle = "black",
  xlabel = "black",
  ylabel = "black",
  watermark = "blue"
)
themeLabelColors$tlf <- list(
  title = "darkblue",
  subtitle = "black",
  xlabel = "black",
  ylabel = "black",
  watermark = "green"
)
themeLabelColors$bw <- list(
  title = "black",
  subtitle = "black",
  xlabel = "black",
  ylabel = "black",
  watermark = "grey50"
)

## -------------------------------------------------
# Define palettes for colors, size for plots
themeAesProperties <- enum(c("default", "tlf", "bw"))
themeAesProperties$default <- list(
  color = seq(1, 10),
  size = seq(1, 10),
  shape = seq(1, 10),
  linetype = seq(1, 10),
  fill = seq(1,10)
)

themeAesProperties$tlf <- list(
  color = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7"),
  size = seq(1, 10),
  shape = c("circle", "square", "diamond", "triangle"),
  linetype = c("solid", "dashed", "dotted")
)
themeAesProperties$tlf$fill <- themeAesProperties$tlf$color

themeAesProperties$bw <- list(
  color = c("#000000", "#999999", "#555555"),
  size = seq(1, 10),
  shape = c("circle", "square", "diamond", "triangle"),
  linetype = c("solid", "dashed", "dotted")
)
themeAesProperties$bw$fill <- themeAesProperties$bw$color
themeAesProperties$big <- themeAesProperties$tlf
themeAesProperties$big$size <- seq(4, 20, 2)

## -------------------------------------------------
# Define PK Ratio Lines Properties
themePKRatioLinesProperties <- data.frame(
  linetype = c("solid", "dashed", "dashed", "dashed", "dashed"),
  color = c("black", "blue", "blue", "red", "red"),
  size = c(2, 1, 1, 1, 1)
)
# Otherwise linetype and color are assumed as levels
themePKRatioLinesProperties$linetype <- as.character(themePKRatioLinesProperties$linetype)
themePKRatioLinesProperties$color <- as.character(themePKRatioLinesProperties$color)

# Black and White properties
themePKRatioLinesPropertiesBW <- themePKRatioLinesProperties
themePKRatioLinesPropertiesBW$color <- as.character(c("#000000", "#555555", "#555555", "#999999", "#999999"))
themePKRatioLinesPropertiesBW$linetype <- as.character(c("solid", "dashed", "dashed", "dotted", "dotted"))

# Big theme properties
themePKRatioLinesPropertiesBig <- themePKRatioLinesProperties
themePKRatioLinesPropertiesBig$size <- themePKRatioLinesProperties$size * 1.5

## -------------------------------------------------
# Define LLOQ Lines Properties
themeLLOQLinesProperties <- list(linetype = "dashed", color = "black", size = 1)
## -------------------------------------------------

#' @title ThemeFont
#' @docType class
#' @description
#' ThemeFont class sets of font properties for texts
#' @include utils.R
#' @export
ThemeFont <- R6::R6Class(
  "ThemeFont",
  public = list(
    titleFont = NULL,
    subtitleFont = NULL,
    xlabelFont = NULL,
    ylabelFont = NULL,
    watermarkFont = NULL,

    initialize = function(labelColors = themeLabelColors$default,
                              labelBaseSize = 14) {

      # Set font properties of labels
      self$titleFont <- Font$new(color = labelColors$title, size = labelBaseSize * 2, fontFace = "bold")
      self$subtitleFont <- Font$new(color = labelColors$subtitle, size = labelBaseSize * 1.5, fontFace = "italic")
      self$xlabelFont <- Font$new(color = labelColors$xlabel, size = labelBaseSize)
      self$ylabelFont <- Font$new(color = labelColors$ylabel, size = labelBaseSize)
      self$watermarkFont <- Font$new(color = labelColors$watermark, size = labelBaseSize)
    }
  )
)


#' @title Theme
#' @docType class
#' @description
#' Theme class sets of font properties for texts
#' @include utils.R
#' @export
Theme <- R6::R6Class(
  "Theme",
  inherit = ThemeFont,
  public = list(
    watermarkText = NULL,
    aesProperties = NULL,

    pkRatioLinesProperties = NULL,
    LLOQLinesProperties = NULL,

    initialize = function(labelColors = themeLabelColors$default,
                              labelBaseSize = 14,
                              watermarkText = "",
                              aesProperties = themeAesProperties$default,
                              pkRatioLinesProperties = themePKRatioLinesProperties,
                              LLOQLinesProperties = themeLLOQLinesProperties) {
      super$initialize(labelColors, labelBaseSize)
      self$watermarkText <- watermarkText

      # Set the color, shape, size maps
      self$aesProperties <- aesProperties

      # Set the properties of specific plots
      self$pkRatioLinesProperties <- pkRatioLinesProperties
      self$LLOQLinesProperties <- LLOQLinesProperties
    }
  )
)

## -------------------------------------------------
# Definition of a few standard themes to be used

defaultTheme <- Theme$new(labelColors = themeLabelColors$default)

tlfTheme <- Theme$new(
  labelColors = themeLabelColors$tlf,
  watermarkText = "tlf-watermark",
  aesProperties = themeAesProperties$tlf
)

bwTheme <- Theme$new(
  labelColors = themeLabelColors$bw,
  watermarkText = "black & white",
  aesProperties = themeAesProperties$bw,
  pkRatioLinesProperties = themePKRatioLinesPropertiesBW
)

bigTheme <- Theme$new(
  labelColors = themeLabelColors$default,
  labelBaseSize = 20,
  watermarkText = "Big",
  aesProperties = themeAesProperties$big,
  pkRatioLinesProperties = themePKRatioLinesPropertiesBig
)

## -------------------------------------------------
#' @title useTheme
#' @param theme
#' Theme to be used as default of the tlf environment
#' @description
#' set the theme to be used as the current default of the tlf environment
#' @export
#'
useTheme <- function(theme) {
  tlfEnv$currentTheme <- theme
}
