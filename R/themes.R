# Theme Properties and theme object definitions
#
## -------------------------------------------------
# Load theme properties from a Json file
load("data/tlfEnvThemesProperties.RData")
# jsonlite::fromJSON("../data/default-themes.json") if imported from a .json


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

    initialize = function(labelColors = tlfEnvThemesProperties$default$labelColors,
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

#' @title ThemeAesProperties
#' @docType class
#' @description
#' Theme aesthetics class to set aesthetic properties plots
#' @include utils.R
#' @export
ThemeAesProperties <- R6::R6Class(
  "ThemeAesProperties",
  public = list(
    color = NULL,
    shape = NULL,
    size = NULL,
    fill = NULL,
    linetype = NULL,
    alpha = NULL,

    initialize = function(aesProperties = tlfEnvThemesProperties$default$aesProperties,
                              color = NULL,
                              shape = NULL,
                              size = NULL,
                              fill = NULL,
                              linetype = NULL,
                              alpha = NULL) {
      self$color <- color %||% aesProperties$color
      self$shape <- shape %||% aesProperties$shape
      self$size <- size %||% aesProperties$size
      self$fill <- fill %||% aesProperties$fill
      self$linetype <- linetype %||% aesProperties$linetype
      self$alpha <- alpha %||% aesProperties$alpha
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
    watermark = NULL,
    background = NULL,
    aesProperties = NULL,

    pkRatioLinesProperties = NULL,
    lloqLinesProperties = NULL,
    histogramLinesProperties = NULL,

    initialize = function(themesProperties = tlfEnvThemesProperties$default,
                              labelColors = NULL,
                              labelBaseSize = 14,
                              background = NULL,
                              watermark = NULL,
                              aesProperties = NULL,
                              pkRatioLinesProperties = NULL,
                              lloqLinesProperties = NULL,
                              histogramLinesProperties = NULL) {
      super$initialize(
        labelColors = labelColors %||% themesProperties$labelColors,
        labelBaseSize = labelBaseSize
      )

      self$background <- background %||% themesProperties$background
      ifnotnull(watermark, self$background$watermark <- watermark)

      # Set the color, shape, size maps
      self$aesProperties <- aesProperties %||% ThemeAesProperties$new(aesProperties = themesProperties$aesProperties)

      # Set the properties of specific plots
      self$pkRatioLinesProperties <- pkRatioLinesProperties %||% themesProperties$pkRatioLinesProperties
      self$lloqLinesProperties <- lloqLinesProperties %||% themesProperties$pkRatioLinesProperties
      self$histogramLinesProperties <- histogramLinesProperties %||% themesProperties$histogramLinesProperties
    }
  )
)

## -------------------------------------------------
# Definition of a few standard themes to be used

#' @title defaultTheme
#' @description
#' Default theme for plot configuration
#' @export
defaultTheme <- Theme$new()

#' @title tlfTheme
#' @description
#' tlf theme for plot configuration
#' @export
tlfTheme <- Theme$new(themesProperties = tlfEnvThemesProperties$tlf)

#' @title bwTheme
#' @description
#' Black and White theme for plot configuration
#' @export
bwTheme <- Theme$new(themesProperties = tlfEnvThemesProperties$bw)

#' @title bigTheme
#' @description
#' Big theme for plot configuration
#' @export
bigTheme <- Theme$new(labelBaseSize = 20, watermark = "Big")

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
