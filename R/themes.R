# Theme Properties and theme object definitions
#
## -------------------------------------------------
# Load theme properties from a Json file
load("data/tlfEnvThemesProperties.RData")
# jsonlite::fromJSON("../data/default-themes.json") if imported from a .json


#' @title ThemeFont
#' @description R6 class defining theme fonts
#' @include utils.R
#' @export
ThemeFont <- R6::R6Class(
  "ThemeFont",
  public = list(
    #' @field titleFont R6 class \code{Font} object
    titleFont = NULL,
    #' @field subtitleFont R6 class \code{Font} object
    subtitleFont = NULL,
    #' @field xlabelFont R6 class \code{Font} object
    xlabelFont = NULL,
    #' @field ylabelFont R6 class \code{Font} object
    ylabelFont = NULL,
    #' @field watermarkFont R6 class \code{Font} object
    watermarkFont = NULL,

    #' @description Create a new \code{ThemeFont} object
    #' @param labelColors list of colors for each label
    #' @param labelBaseSize numeric value for theme base size
    #' @return A new \code{ThemeFont} object
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
#' @description R6 class defining theme aesthetic properties plots
#' @include utils.R
#' @export
ThemeAesProperties <- R6::R6Class(
  "ThemeAesProperties",
  public = list(
    #' @field color character vector of color properties
    color = NULL,
    #' @field shape character vector of shape properties
    shape = NULL,
    #' @field size numeric vector of size properties
    size = NULL,
    #' @field fill character vector of fill properties
    fill = NULL,
    #' @field linetype character vector of linetype properties
    linetype = NULL,
    #' @field alpha numeric vector of alpha properties
    alpha = NULL,

    #' @description Create a new \code{ThemeAesProperties} object
    #' @param aesProperties list of aesthetic properties
    #' @param color character vector of color properties
    #' @param shape character vector of shape properties
    #' @param size numeric vector of size properties
    #' @param fill character vector of fill properties
    #' @param linetype character vector of linetype properties
    #' @param alpha numeric vector of alpha properties
    #' @return A new \code{ThemeAesProperties} object
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
#' @description R6 class defining theme
#' @include utils.R
#' @export
Theme <- R6::R6Class(
  "Theme",
  inherit = ThemeFont,
  public = list(
    #' @field watermark list of font properties for watermark
    watermark = NULL,
    #' @field background list of aesthetic properties for background configuration
    background = NULL,
    #' @field aesProperties list of aesthetic properties for plots in general
    aesProperties = NULL,
    #' @field defaultCaption aesthetic properties for specific plots
    defaultCaption = NULL,

    #' @description Create a new \code{Theme} object
    #' @param themesProperties list of aesthetic properties
    #' @param labelColors list of colors for each label
    #' @param labelBaseSize numeric value for theme base size
    #' @param background list of aesthetic properties for background configuration
    #' @param watermark list of font properties for watermark
    #' @param aesProperties list of aesthetic properties for plots in general
    #' @param defaultCaption list of aesthetic properties for specific plot features
    #' @return A new \code{Theme} object
    initialize = function(themesProperties = tlfEnvThemesProperties$default,
                              labelColors = NULL,
                              labelBaseSize = 14,
                              background = NULL,
                              watermark = NULL,
                              aesProperties = NULL,
                              defaultCaption = NULL) {
      super$initialize(
        labelColors = labelColors %||% themesProperties$labelColors,
        labelBaseSize = labelBaseSize
      )

      self$background <- background %||% themesProperties$background
      ifnotnull(watermark, self$background$watermark <- watermark)

      # Set the color, shape, size maps
      self$aesProperties <- aesProperties %||% ThemeAesProperties$new(aesProperties = themesProperties$aesProperties)

      # Set the properties of specific plots
      self$defaultCaption <- defaultCaption %||% themesProperties$defaultCaption
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

## -------------------------------------------------
#' @title runThemeMaker
#' @description
#' Run shiny app that allows easy setting of Theme objects.
#' Theme objects drive default properties of plots
#' @export
#'
runThemeMaker <- function() {
  appPath <- system.file("theme-maker", package = "tlf")
  shiny::runApp(appPath)
}
