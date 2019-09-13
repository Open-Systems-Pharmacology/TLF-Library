#' @title Theme
#' @docType class
#' @description
#' Theme class sets of font properties for texts
#' @include utils.R
#' @export
Theme <- R6::R6Class(
  "Theme",
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    watermark = NULL,

    initialize = function(colorPalette = labelColorPalette$default, baseSize=12) {
      self$title <- Font$new(color = colorPalette$title, size = baseSize * 2, fontFace = "bold")
      self$subtitle <- Font$new(color = colorPalette$subtitle, size = baseSize * 1.5, fontFace = "italic")
      self$xlabel <- Font$new(color = colorPalette$xlabel, size = baseSize)
      self$ylabel <- Font$new(color = colorPalette$ylabel, size = baseSize)
      self$watermark <- Font$new(color = colorPalette$watermark, size = baseSize)
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

# Define color palettes for plot labels
labelColorPalette <- enum(c("default", "tlf", "bw"))
labelColorPalette$default <- list(title = "darkblue", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "blue")
labelColorPalette$tlf <- list(title = "darkblue", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "green")
labelColorPalette$bw <- list(title = "black", subtitle = "black", xlabel = "black", ylabel = "black", watermark = "grey50")

# Definition of default theme
defaultTheme <- Theme$new(colorPalette = labelColorPalette$default, baseSize = 12)

# Definition of tlf theme
tlfTheme <- Theme$new(colorPalette = labelColorPalette$tlf, baseSize = 12)

# Definition of black and white theme
bwTheme <- Theme$new(colorPalette = labelColorPalette$bw, baseSize = 12)

# Definition of black and white theme
bigTheme <- Theme$new(colorPalette = labelColorPalette$default, baseSize = 16)

useTheme <- function(theme) {
  tlfEnv$currentTheme <- theme
}
