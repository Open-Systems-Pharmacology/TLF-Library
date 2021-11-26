#' @title loadThemeFromJson
#' @description Load theme object from json file.
#' A template of a json theme is available at system.file(package= "tlf", "theme-maker","theme-template.json")
#' @param jsonFile path of json file
#' @return A `Theme` object
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
      if (isIncluded(themeProperty, "aestheticMaps")) {
        propertyExpression <- parse(text = paste0(
          themeProperty, "$", propertyField, " <- themeContent$", themeProperty, "$", propertyField
        ))
        eval(propertyExpression)
      }
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

#' @title useTemplateTheme
#' @description
#' Set default theme to be used as the current default of the tlf environment
#' @export
#' @examples
#' useTemplateTheme()
#' addScatter(x = seq(1,10), y = rnorm(10))
useTemplateTheme <- function() {
  useTheme(loadThemeFromJson(system.file("themes", "template-theme.json", package = "tlf")))
}

#' @title useRETheme
#' @description
#' Set default Reporting Engine theme to be used as the current default of the tlf environment
#' @export
#' @examples 
#' useRETheme()
#' addScatter(x = seq(1,10), y = rnorm(10))
useRETheme <- function() {
  useTheme(loadThemeFromJson(system.file("themes", "re-theme.json", package = "tlf")))
}

#' @title useExcelTheme
#' @description
#' Set default Excel theme to be used as the current default of the tlf environment
#' @export
#' @examples 
#' useExcelTheme()
#' addScatter(x = seq(1,10), y = rnorm(10))
useExcelTheme <- function() {
  useTheme(loadThemeFromJson(system.file("themes", "excel-theme.json", package = "tlf")))
}

#' @title useHighChartTheme
#' @description
#' Set default HighChart theme to be used as the current default of the tlf environment
#' @export
#' @examples 
#' useHighChartTheme()
#' addScatter(x = seq(1,10), y = rnorm(10))
useHighChartTheme <- function() {
  useTheme(loadThemeFromJson(system.file("themes", "hc-theme.json", package = "tlf")))
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

## -------------------------------------------------
#' @title runPlotMaker
#' @description
#' Run shiny app that allows to easily make plots from user interface.
#' @export
runPlotMaker <- function() {
  appPath <- system.file("plot-maker", package = "tlf")
  shiny::runApp(appPath)
}
