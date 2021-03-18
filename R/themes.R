# Theme Properties and theme object definitions

#' @title ThemeFont
#' @description R6 class defining theme font properties
#' @field title \code{Font} object for font properties title
#' @field subtitle \code{Font} object for font properties of subtitle
#' @field xlabel \code{Font} object for font properties of xlabel
#' @field ylabel \code{Font} object for font properties of ylabel
#' @field watermark \code{Font} object for font properties of watermark
#' @field legendTitle \code{Font} object for font properties of legend title
#' @field legend \code{Font} object for font properties of legend
#' @field xAxis \code{Font} object for font properties of xAxis
#' @field yAxis \code{Font} object for font properties of yAxis
#' @export
ThemeFont <- R6::R6Class(
  "ThemeFont",
  
  public = list(
    title = NULL,
    subtitle = NULL,
    xlabel = NULL,
    ylabel = NULL,
    watermark = NULL,
    legendTitle = NULL,
    legend = NULL,
    xAxis = NULL,
    yAxis = NULL,

    #' @description Create a new \code{ThemeFont} object
    #' @param title \code{Font} object or list for font properties title
    #' @param subtitle \code{Font} object or list for font properties of subtitle
    #' @param xlabel \code{Font} object or list for font properties of xlabel
    #' @param ylabel \code{Font} object or list for font properties of ylabel
    #' @param watermark \code{Font} object or list for font properties of watermark
    #' @param legendTitle \code{Font} object or list for font properties of legend
    #' @param legend \code{Font} object or list for font properties of legend
    #' @param xAxis \code{Font} object or list for font properties of xAxis
    #' @param yAxis \code{Font} object or list for font properties of yAxis
    #' @param baseColor name of base color of undefined fonts. Default is black.
    #' @param baseSize base size of undefined fonts. Default is 12.
    #' @param baseFace name of base face of undefined fonts. Default is "plain".
    #' @param baseFamily name of base family of undefined fonts. Default is "".
    #' @param baseAngle base angle of undefined fonts. Default is 0 degree.
    #' @return A new \code{ThemeFont} object
    initialize = function(title = NULL,
                              subtitle = NULL,
                              xlabel = NULL,
                              ylabel = NULL,
                              watermark = NULL,
                              legendTitle = NULL,
                              legend = NULL,
                              xAxis = NULL,
                              yAxis = NULL,
                              baseColor = "black",
                              baseSize = 12,
                              baseFace = "plain",
                              baseFamily = "",
                              baseAngle = 0) {
      # Validate necessary input
      validateIsString(baseColor)
      validateIsString(baseFace)
      validateIsString(baseFamily)
      validateIsNumeric(baseSize)
      validateIsNumeric(baseAngle)

      # Create all field properties by parsing and evaluating their expression
      fieldNames <- c("title", "subtitle", "xlabel", "ylabel", "watermark", "legendTitle", "legend", "xAxis", "yAxis")
      setFontExpression <- parse(text = paste0("self$", fieldNames, " <- Font$new(
                                               color = ", fieldNames, "$color %||% baseColor, 
                                               size = ", fieldNames, "$size %||% baseSize, 
                                               fontFace = ", fieldNames, "$fontFace %||% baseFace, 
                                               fontFamily = ", fieldNames, "$fontFamily %||% baseFamily, 
                                               angle = ", fieldNames, "$angle %||% baseAngle)"))
      eval(setFontExpression)
    },

    #' @description Translate object into a json list
    #' @return A list that can be saved into a json file
    toJson = function() {
      jsonObject <- list()
      fieldNames <- c("title", "subtitle", "xlabel", "ylabel", "watermark", "legendTitle", "legend", "xAxis", "yAxis")
      setJsonExpression <- parse(text = paste0("jsonObject$", fieldNames, " <- list(
                                               color = self$", fieldNames, "$color, 
                                               size = self$", fieldNames, "$size, 
                                               angle = self$", fieldNames, "$angle, 
                                               fontFace = self$", fieldNames, "$fontFace, 
                                               fontFamily = self$", fieldNames, "$fontFamily)"))
      eval(setJsonExpression)
      return(jsonObject)
    }
  )
)

#' @title ThemeBackground
#' @description R6 class defining theme background properties
#' @field watermark character defining content of watermark
#' @field legendPosition character defining where legend should usually be placed
#' @field plot \code{BackgroundElement} object for plot area properties (outside of panel)
#' @field panel \code{BackgroundElement} object for plot area properties (inside of panel)
#' @field xAxis \code{BackgroundElement} object for x axis properties
#' @field yAxis \code{BackgroundElement} object for y axis properties
#' @field xGrid \code{BackgroundElement} object for x grid properties
#' @field yGrid \code{BackgroundElement} object for y grid properties
#' @field legend \code{BackgroundElement} object for legend area properties
#' @export
ThemeBackground <- R6::R6Class(
  "ThemeBackground",
  
  public = list(
    watermark = NULL,
    legendPosition = NULL,
    plot = NULL,
    panel = NULL,
    xAxis = NULL,
    yAxis = NULL,
    xGrid = NULL,
    yGrid = NULL,
    legend = NULL,

    #' @description Create a new \code{ThemeBackground} object
    #' @param watermark character defining content of watermark
    #' @param legendPosition character defining where legend should usually be placed
    #' @param plot \code{BackgroundElement} object or list for plot area properties (outside of panel)
    #' @param panel \code{BackgroundElement} object or list for plot area properties (inside of panel)
    #' @param xAxis \code{BackgroundElement} object or list for x axis properties
    #' @param yAxis \code{BackgroundElement} object or list for y axis properties
    #' @param xGrid \code{BackgroundElement} object or list for x grid properties
    #' @param yGrid \code{BackgroundElement} object or list for y grid properties
    #' @param legend \code{BackgroundElement} object or list for legend area properties
    #' @param baseFill name of base color fill of undefined background elements. Default is white.
    #' @param baseColor name of base color of undefined background elements. Default is black.
    #' @param baseSize name of base size of undefined background elements. Default is 0.5.
    #' @param baseLinetype name of base size of undefined background elements. Default is "solid".
    #' @return A new \code{ThemeFont} object
    initialize = function(watermark = NULL,
                              legendPosition = NULL,
                              plot = NULL,
                              panel = NULL,
                              xAxis = NULL,
                              yAxis = NULL,
                              xGrid = NULL,
                              yGrid = NULL,
                              legend = NULL,
                              baseFill = "white",
                              baseColor = "black",
                              baseSize = 0.5,
                              baseLinetype = "solid") {
      # Validate necessary input
      validateIsString(baseFill)
      validateIsString(baseColor)
      validateIsString(baseLinetype)
      validateIsNumeric(baseSize)

      self$watermark <- watermark %||% ""
      self$legendPosition <- legendPosition %||% LegendPositions$outsideRight

      # Create all field properties by parsing and evaluating their expression
      areaFieldNames <- c("plot", "panel", "legend")
      lineFieldNames <- c("xAxis", "yAxis", "xGrid", "yGrid")

      setAreaExpression <- parse(text = paste0("self$", areaFieldNames, " <- BackgroundElement$new(fill = ", areaFieldNames, "$fill %||% baseFill, color = ", areaFieldNames, "$color %||% baseColor, size = ", areaFieldNames, "$size %||% baseSize, linetype = ", areaFieldNames, "$linetype %||% baseLinetype)"))
      setLineExpression <- parse(text = paste0("self$", lineFieldNames, " <- LineElement$new(color = ", lineFieldNames, "$color %||% baseColor, size = ", lineFieldNames, "$size %||% baseSize, linetype = ", lineFieldNames, "$linetype %||% baseLinetype)"))
      eval(setAreaExpression)
      eval(setLineExpression)
    },

    #' @description Translate object into a json list
    #' @return A list that can be saved into a json file
    toJson = function() {
      jsonObject <- list()
      jsonObject$watermark <- self$watermark
      jsonObject$legendPosition <- self$legendPosition
      areaFieldNames <- c("plot", "panel", "legend")
      lineFieldNames <- c("xAxis", "yAxis", "xGrid", "yGrid")

      setJsonAreaExpression <- parse(text = paste0("jsonObject$", areaFieldNames, " <- list(
                                                   fill = self$", areaFieldNames, "$fill, 
                                                   color = self$", areaFieldNames, "$color, 
                                                   size = self$", areaFieldNames, "$size, 
                                                   linetype = self$", areaFieldNames, "$linetype)"))
      setJsonLineExpression <- parse(text = paste0("jsonObject$", lineFieldNames, " <- list(
                                                   color = self$", lineFieldNames, "$color, 
                                                   size = self$", lineFieldNames, "$size, 
                                                   linetype = self$", lineFieldNames, "$linetype)"))
      eval(setJsonAreaExpression)
      eval(setJsonLineExpression)
      return(jsonObject)
    }
  )
)

#' @title ThemeAestheticMaps
#' @description R6 class defining theme aesthetic maps
#' @field color color map as character or numeric vector
#' @field fill fill map as character or numeric vector
#' @field size size map as numeric vector
#' @field shape shape map as numeric vector
#' @field linetype linetype as character vector
#' @field alpha map as numeric vector
#' @export
ThemeAestheticMaps <- R6::R6Class(
  "ThemeAestheticMaps",
  
  public = list(
    color = NULL,
    fill = NULL,
    shape = NULL,
    size = NULL,
    linetype = NULL,
    alpha = NULL,

    #' @description Create a new \code{ThemeAestheticMaps} object
    #' @param color color map as list, character or numeric vector
    #' @param fill fill map as list, character or numeric vector
    #' @param shape shape map as list, character or numeric vector
    #' @param size size  map as list, character or numeric vector
    #' @param linetype linetype map as list, character or numeric vector
    #' @param alpha alpha map as list, character or numeric vector
    #' @return A new \code{ThemeAestheticMaps} object
    initialize = function(color = NULL,
                              fill = NULL,
                              shape = NULL,
                              size = NULL,
                              linetype = NULL,
                              alpha = NULL) {

      # Validate necessary input
      validateIsString(color, nullAllowed = TRUE)
      validateIsString(fill, nullAllowed = TRUE)
      validateIsString(linetype, nullAllowed = TRUE)

      validateIsOfType(shape, c("numeric", "character"), nullAllowed = TRUE)

      validateIsNumeric(size, nullAllowed = TRUE)
      validateIsNumeric(alpha, nullAllowed = TRUE)

      # Default aesthetic maps
      self$color <- color %||% ColorMaps$default
      self$fill <- fill %||% ColorMaps$default
      self$shape <- shape %||% as.numeric(Shapes)
      self$linetype <- linetype %||% as.character(Linetypes)
      self$size <- size %||% seq(1, 5)
      self$alpha <- alpha %||% c(0.75, 0.5, 0.25)

      # Checks shapes and linetype according to ggplot2 standards
      self$shape <- asPlotShape(self$shape)
      validateIsIncluded(self$linetype, Linetypes)
    },

    #' @description Translate object into a json list
    #' @return A list that can be saved into a json file
    toJson = function() {
      jsonObject <- list()
      fieldNames <- names(AestheticProperties)

      setJsonExpression <- parse(text = paste0("jsonObject$", fieldNames, " <- self$", fieldNames))
      eval(setJsonExpression)
      return(jsonObject)
    }
  )
)

#' @title ThemeAestheticSelections
#' @description R6 class defining how plot configurations will use aesthetic maps
#' @export
ThemeAestheticSelections <- R6::R6Class(
  "ThemeAestheticSelections",
  inherit = ThemeAestheticMaps,
  public = list(

    #' @description Create a new \code{ThemeAestheticSelections} object
    #' @param color selection key or values for choice of color
    #' @param fill selection key or values for choice of fill
    #' @param shape selection key or values for choice of shape
    #' @param size selection key or values for choice of size
    #' @param linetype selection key or values for choice of linetype
    #' @param alpha selection key or values for choice of alpha
    #' @return A new \code{ThemeAestheticSelections} object
    initialize = function(color = NULL,
                              fill = NULL,
                              shape = NULL,
                              size = NULL,
                              linetype = NULL,
                              alpha = NULL) {

      # Associate to each field its value
      initializeExpression <- parse(text = paste0("self$", names(AestheticProperties), " <- ", names(AestheticProperties), " %||% 'first'"))
      eval(initializeExpression)
    },

    #' @description Translate object into a json list
    #' @return A list that can be saved into a json file
    toJson = function() {
      jsonObject <- list()
      fieldNames <- names(AestheticProperties)

      setJsonExpression <- parse(text = paste0("jsonObject$", fieldNames, " <- self$", fieldNames))
      eval(setJsonExpression)
      return(jsonObject)
    }
  )
)

#' @title ThemePlotConfigurations
#' @description R6 class defining theme of plot configuration objects
#' @field addScatter theme properties for `PlotConfiguration` objects as used in function `addScatter()`
#' @field addLine theme properties for `PlotConfiguration` objects as used in function `addLine()`
#' @field addRibbon theme properties for `PlotConfiguration` objects as used in function `addRibbon()`
#' @field addErrorbar theme properties for `PlotConfiguration` objects as used in function `addErrorbar()`
#' @field plotPKRatio theme properties for `PlotConfiguration` objects as used in function `plotPKRatio()`
#' @field plotDDIRatio theme properties for `PlotConfiguration` objects as used in function `plotDDIRatio()`
#' @field plotTimeProfile theme properties for `PlotConfiguration` objects as used in function `plotTimeProfile()`
#' @field plotObsVsPred theme properties for `PlotConfiguration` objects as used in function `plotObsVsPred()`
#' @field plotBoxWhisker theme properties for `PlotConfiguration` objects as used in function `plotBoxWhisker()`
#' @field plotTornado theme properties for `PlotConfiguration` objects as used in function `plotTornado()`
#' @field plotHistogram theme properties for `PlotConfiguration` objects as used in function `plotHistogram()`
#' @export
ThemePlotConfigurations <- R6::R6Class(
  "ThemePlotConfigurations",
  
  public = list(
    addScatter = NULL,
    addLine = NULL,
    addRibbon = NULL,
    addErrorbar = NULL,
    plotPKRatio = NULL,
    plotDDIRatio = NULL,
    plotTimeProfile = NULL,
    plotObsVsPred = NULL,
    plotBoxWhisker = NULL,
    plotTornado = NULL,
    plotHistogram = NULL,

    #' @description Create a new \code{ThemePlotConfigurations} object
    #' @param addScatter theme properties for `PlotConfiguration` objects as used in function `addScatter()`
    #' @param addLine theme properties for `PlotConfiguration` objects as used in function `addLine()`
    #' @param addRibbon theme properties for `PlotConfiguration` objects as used in function `addRibbon()`
    #' @param addErrorbar theme properties for `PlotConfiguration` objects as used in function `addErrorbar()`
    #' @param plotPKRatio theme properties for `PlotConfiguration` objects as used in function `plotPKRatio()`
    #' @param plotDDIRatio theme properties for `PlotConfiguration` objects as used in function `plotDDIRatio()`
    #' @param plotTimeProfile theme properties for `PlotConfiguration` objects as used in function `plotTimeProfile()`
    #' @param plotObsVsPred theme properties for `PlotConfiguration` objects as used in function `plotObsVsPred()`
    #' @param plotBoxWhisker theme properties for `PlotConfiguration` objects as used in function `plotBoxWhisker()`
    #' @param plotTornado theme properties for `PlotConfiguration` objects as used in function `plotTornado()`
    #' @param plotHistogram theme properties for `PlotConfiguration` objects as used in function `plotHistogram()`
    #' @return A new \code{ThemePlotConfigurations} object
    initialize = function(addScatter = NULL,
                              addLine = NULL,
                              addRibbon = NULL,
                              addErrorbar = NULL,
                              plotPKRatio = NULL,
                              plotDDIRatio = NULL,
                              plotTimeProfile = NULL,
                              plotObsVsPred = NULL,
                              plotBoxWhisker = NULL,
                              plotTornado = NULL,
                              plotHistogram = NULL) {

      # Validate necessary input
      atomPlotInputs <- c("addScatter", "addLine", "addRibbon", "addErrorbar")
      moleculePlotInputs <- c("plotPKRatio", "plotDDIRatio", "plotTimeProfile", "plotObsVsPred", "plotBoxWhisker", "plotTornado", "plotHistogram")

      validateExpressions <- parse(text = paste0("validateIsOfType(", atomPlotInputs, ", 'ThemeAestheticSelections', nullAllowed = TRUE)"))
      eval(validateExpressions)
      validateExpressions <- parse(text = paste0("validateIsOfType(c(", moleculePlotInputs, "), 'ThemeAestheticSelections', nullAllowed = TRUE)"))
      eval(validateExpressions)

      # Default aesthetic for atom plots
      self$addScatter <- addScatter %||% ThemeAestheticSelections$new(color = "next", fill = NA, shape = "next", linetype = "blank", size = "first", alpha = 1)
      self$addLine <- addLine %||% ThemeAestheticSelections$new(color = "next", fill = NA, shape = "blank", linetype = "reset", size = "first", alpha = 1)
      self$addRibbon <- addRibbon %||% ThemeAestheticSelections$new(color = "next", fill = "next", shape = "blank", linetype = "first", size = "same", alpha = 1)
      self$addErrorbar <- addErrorbar %||% ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "first", size = "same", alpha = 1)

      # Default aesthetic for molecule plots
      self$plotPKRatio <- plotPKRatio %||% list(
        lines = ThemeAestheticSelections$new(color = c("#000000", "#0078D7", "#D83B01"), linetype = c("longdash", "longdash", "longdash"), size = 0.5, alpha = 1),
        points = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "reset", linetype = "blank", size = 3),
        errorbars = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "solid", size = 1)
      )
      self$plotDDIRatio <- plotDDIRatio %||% list(
        lines = ThemeAestheticSelections$new(color = c("#000000", "#0078D7", "#D83B01"), linetype = c("longdash", "longdash", "longdash"), size = 0.5, alpha = 1),
        points = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "reset", linetype = "blank", size = 3),
        errorbars = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "solid", size = 1)
      )
      self$plotTimeProfile <- plotTimeProfile %||% list(
        lines = ThemeAestheticSelections$new(color = "reset", fill = "reset", shape = "blank", linetype = "reset", size = 1),
        ribbons = ThemeAestheticSelections$new(color = "reset", fill = "reset", shape = "blank", linetype = "blank", size = 1, alpha = "first"),
        points = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "reset", linetype = "blank", size = 3),
        errorbars = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "solid", size = 1)
      )
      self$plotObsVsPred <- plotObsVsPred %||% list(
        lines = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "reset", size = 1),
        points = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "reset", linetype = "blank", size = 3),
        errorbars = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "solid", size = 1)
      )
      self$plotBoxWhisker <- plotBoxWhisker %||% list(
        ribbons = ThemeAestheticSelections$new(color = "#000000", fill = "next", linetype = "solid", size = 1, alpha = "first"),
        points = ThemeAestheticSelections$new(color = "#000000", shape = "first", linetype = "blank", size = 1)
      )
      self$plotTornado <- plotTornado %||% list(
        lines = ThemeAestheticSelections$new(color = "#000000", fill = NA, shape = "blank", linetype = "longdash", size = 1),
        ribbons = ThemeAestheticSelections$new(color = "reset", fill = "reset", shape = "blank", linetype = "solid", size = 1, alpha = "first"),
        points = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "reset", linetype = "blank", size = 3)
      )
      self$plotHistogram <- plotHistogram %||% list(
        lines = ThemeAestheticSelections$new(color = "reset", fill = NA, shape = "blank", linetype = "reset", size = 1),
        ribbons = ThemeAestheticSelections$new(color = "#000000", fill = "reset", shape = "blank", linetype = "solid", size = 0.5, alpha = "first")
      )
    },

    #' @description Translate object into a json list
    #' @return A list that can be saved into a json file
    toJson = function() {
      jsonObject <- list()
      fieldNames <- c("addScatter", "addLine", "addRibbon", "addErrorbar")

      setJsonExpression <- parse(text = paste0("jsonObject$", fieldNames, " <- self$", fieldNames, "$toJson()"))
      eval(setJsonExpression)
      return(jsonObject)
    }
  )
)

#' @title Theme
#' @description R6 class defining theme properties
#' @export
Theme <- R6::R6Class(
  "Theme",
  public = list(

    #' @description Create a new \code{Theme} object
    #' @param fonts `ThemeFont` object
    #' @param background `ThemeBackground` object
    #' @param aestheticMaps `ThemeAestheticMaps` object
    #' @param plotConfigurations `ThemePlotConfiguration` object
    #' @return A new \code{Theme} object
    initialize = function(fonts = NULL,
                              background = NULL,
                              aestheticMaps = NULL,
                              plotConfigurations = NULL) {
      validateIsOfType(fonts, "ThemeFont", nullAllowed = TRUE)
      validateIsOfType(background, "ThemeBackground", nullAllowed = TRUE)
      validateIsOfType(aestheticMaps, "ThemeAestheticMaps", nullAllowed = TRUE)
      validateIsOfType(plotConfigurations, "ThemePlotConfigurations", nullAllowed = TRUE)

      private$.fonts <- fonts %||% ThemeFont$new()
      private$.background <- background %||% ThemeBackground$new()
      private$.aestheticMaps <- aestheticMaps %||% ThemeAestheticMaps$new()
      private$.plotConfigurations <- plotConfigurations %||% ThemePlotConfigurations$new()
    },

    #' @description Save `Theme` as a json file
    #' @param jsonFile name of json file
    save = function(jsonFile) {
      validateIsString(jsonFile)
      themeContent <- list(
        font = private$.fonts$toJson(),
        background = private$.background$toJson(),
        aestheticMaps = private$.aestheticMaps$toJson(),
        plotConfigurations = private$.plotConfigurations$toJson()
      )
      # Options added in saved json are to help users reading the output
      jsonContent <- jsonlite::toJSON(themeContent, auto_unbox = TRUE, pretty = TRUE)
      write(jsonContent, file = jsonFile)
      return(invisible())
    }
  ),
  active = list(
    #' @field fonts `ThemeFont` object
    fonts = function(value) {
      if (missing(value)) {
        return(private$.fonts)
      }
      validateIsOfType(value, "ThemeFont")
      private$.fonts <- value
    },
    #' @field background `ThemeBackground` object
    background = function(value) {
      if (missing(value)) {
        return(private$.background)
      }
      validateIsOfType(value, "ThemeBackground")
      private$.background <- value
    },
    #' @field aestheticMaps `ThemeAestheticMaps` object
    aestheticMaps = function(value) {
      if (missing(value)) {
        return(private$.aestheticMaps)
      }
      validateIsOfType(value, "ThemeAestheticMaps")
      private$.aestheticMaps <- value
    },
    #' @field plotConfigurations `ThemePlotConfiguration` object
    plotConfigurations = function(value) {
      if (missing(value)) {
        return(private$.plotConfigurations)
      }
      validateIsOfType(value, "ThemePlotConfigurations")
      private$.plotConfigurations <- value
    }
  ),
  private = list(
    .fonts = NULL,
    .background = NULL,
    .aestheticMaps = NULL,
    .plotConfigurations = NULL
  )
)

#' @title loadThemeFromJson
#' @description Load theme object from json file.
#' A template of a json theme is available at system.file(package= "tlf", "theme-maker","theme-template.json")
#' @param jsonFile path of json file
#' @return A \code{Theme} object
#' @export
#' @import jsonlite
loadThemeFromJson <- function(jsonFile) {
  # Get the content of json file and define lists of its properties
  themeContent <- jsonlite::fromJSON(jsonFile)
  themeProperties <- names(themeContent)

  # Define default `Theme` properties, properties defined in json will overwrite those
  fonts <- ThemeFont$new()
  background <- ThemeBackground$new()
  aestheticMaps <- ThemeAestheticMaps$new()
  plotConfigurations <- ThemePlotConfigurations$new()

  for (themeProperty in themeProperties) {
    if (!isIncluded(themeProperty, c("fonts", "background", "aestheticMaps", "plotConfigurations"))) {
      next
    }
    propertyFields <- names(themeContent[[themeProperty]])
    for (propertyField in propertyFields) {
      inputs <- names(themeContent[[themeProperty]][[propertyField]])
      if (isOfLength(inputs, 0)) {
        next
      }
      # Expressions overwriting the properties:
      # For each theme property (w.g. fonts, background ...), associate its values (e.g. size, color ...)
      propertyExpression <- parse(text = paste0(
        themeProperty, "$", propertyField, "$", inputs,
        " <- themeContent$", themeProperty, "$", propertyField, "$", inputs
      ))
      eval(propertyExpression)
    }
    # Some specific cases are missing from the expressions
    background$watermark <- themeContent$background$watermark
    background$legendPosition <- themeContent$background$legendPosition
  }
  return(Theme$new(
    fonts = fonts,
    background = background,
    aestheticMaps = aestheticMaps,
    plotConfigurations = plotConfigurations
  ))
}


#' @title saveThemeToJson
#' @description Save theme object to a json file.
#' @param jsonFile path of json file
#' @param theme `Theme` object path of json file
#' @export
saveThemeToJson <- function(jsonFile, theme = NULL) {
  # Check that theme is a Theme
  validateIsOfType(theme, "Theme", nullAllowed = TRUE)
  validateIsString(jsonFile)

  if (isOfLength(theme, 0)) {
    theme <- tlfEnv$currentTheme
  }
  theme$save(jsonFile)
  return(invisible())
}

## -------------------------------------------------
#' @title useTheme
#' @param theme
#' Theme to be used as default of the tlf environment
#' @description
#' set the theme to be used as the current default of the tlf environment
#' @export
#'
useTheme <- function(theme) {
  validateIsOfType(theme, "Theme")
  tlfEnv$currentTheme <- theme
}

## -------------------------------------------------
#' @title runThemeMaker
#' @description
#' Run shiny app that allows easy setting of Theme objects.
#' Theme objects drive default properties of plots
#' @export
runThemeMaker <- function() {
  appPath <- system.file("theme-maker", package = "tlf")
  shiny::runApp(appPath)
}
