#---------- Initialization of app ----------#
# Load required packages
require(shiny)
require(tlf)

# Load initial template theme
jsonTheme <- loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf"))
useTheme(jsonTheme)

# Load sample data
testData <- read.csv(system.file("extdata", "test-data.csv", package = "tlf"))
testData$ymin <- testData$Ratio - testData$SD
testData$ymax <- testData$Ratio + testData$SD

# Helper: list of available plots
listOfAvailablePlots <- c("initializePlot", "addScatter", "addLine", "addRibbon", "addErrorbar",
                          "plotPKRatio", "plotDDIRatio", "plotBoxWhisker", "plotHistogram", 
                          "plotTimeProfile", "plotTornado", "plotObsVsPred")


#---------- User interface ----------#
ui <- fluidPage(
  h1("Theme Maker", align = "center"),
  column(
    5,
    tabsetPanel(
      tabPanel(
        "Labels",
        tabsetPanel(
          tabPanel("Title",
                   fluidRow(
                     textInput("titleColor", label = "Color", value = jsonTheme$fonts$title$color),
                     sliderInput("titleSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$title$size, step = 0.5),
                     sliderInput("titleAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$title$angle, step = 5)
                   )),
          tabPanel("Subtitle",
                   fluidRow(
                     textInput("subtitleColor", label = "Color", value = jsonTheme$fonts$subtitle$color),
                     sliderInput("subtitleSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$subtitle$size, step = 0.5),
                     sliderInput("subtitleAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$subtitle$angle, step = 5)
                   )),
          tabPanel("X-label",
                   fluidRow(
                     textInput("xlabelColor", label = "Color", value = jsonTheme$fonts$xlabel$color),
                     sliderInput("xlabelSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$xlabel$size, step = 0.5),
                     sliderInput("xlabelAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$xlabel$angle, step = 5)
                   )),
          tabPanel("Y-label",
                   fluidRow(
                     textInput("ylabelColor", label = "Color", value = jsonTheme$fonts$ylabel$color),
                     sliderInput("ylabelSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$ylabel$size, step = 0.5),
                     sliderInput("ylabelAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$ylabel$angle, step = 5)
                   ))
        )),
      tabPanel(
        "Watermark",
        fluidRow(
          textInput("watermarkText", label = "Content", value = jsonTheme$background$watermark),
          textInput("watermarkColor", label = "Color", value = jsonTheme$fonts$watermark$color),
          sliderInput("watermarkSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$watermark$size, step = 0.5),
          sliderInput("watermarkAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$watermark$angle, step = 5)
        )
      ),
      tabPanel(
        "Background",
        tabsetPanel(
          tabPanel("Plot Area",
                   textInput("plotFill", label = "Fill", value = jsonTheme$background$plot$fill),
                   textInput("plotColor", label = "Color", value = jsonTheme$background$plot$color),
                   sliderInput("plotSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$plot$size, step = 0.05),
                   selectInput("plotLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$plot$linetype)
                   ),
          tabPanel("Panel Area",
                   textInput("panelFill", label = "Fill", value = jsonTheme$background$panel$fill),
                   textInput("panelColor", label = "Color", value = jsonTheme$background$panel$color),
                   sliderInput("panelSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$panel$size, step = 0.05),
                   selectInput("panelLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$panel$linetype)
                   )
          )),
      tabPanel(
        "Axes",
        tabsetPanel(
          tabPanel("X-axis",
                   textInput("xAxisColor", label = "Color", value = jsonTheme$background$xAxis$color),
                   sliderInput("xAxisSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$xAxis$size, step = 0.05),
                   selectInput("xAxisLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$xAxis$linetype),
                   h3("Ticks"),
                   textInput("xAxisTicksColor", label = "Color", value = jsonTheme$fonts$xAxis$color),
                   sliderInput("xAxisTicksSize", label = "Size", min = 0, max = 30, value = jsonTheme$fonts$xAxis$size, step = 0.5),
                   sliderInput("xAxisTicksAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$xAxis$angle, step = 5)
                   ),
          tabPanel("Y-axis",
                   textInput("yAxisColor", label = "Color", value = jsonTheme$background$yAxis$color),
                   sliderInput("yAxisSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$yAxis$size, step = 0.05),
                   selectInput("yAxisLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$yAxis$linetype),
                   h3("Ticks"),
                   textInput("yAxisTicksColor", label = "Color", value = jsonTheme$fonts$xAxis$color),
                   sliderInput("yAxisTicksSize", label = "Size", min = 0, max = 30, value = jsonTheme$fonts$yAxis$size, step = 0.5),
                   sliderInput("yAxisTicksAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$yAxis$angle, step = 5)
                   )
          )),
      tabPanel(
        "Grid",
        tabsetPanel(
          tabPanel("X-grid",
                   textInput("xGridColor", label = "Color", value = jsonTheme$background$xGrid$color),
                   sliderInput("xGridSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$xGrid$size, step = 0.05),
                   selectInput("xGridLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$xGrid$linetype)
                   ),
          tabPanel("Y-Grid",
                   textInput("yGridColor", label = "Color", value = jsonTheme$background$yGrid$color),
                   sliderInput("yGridSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$yGrid$size, step = 0.05),
                   selectInput("yGridLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$yGrid$linetype)
                   )
          )),
      tabPanel(
        "Legend",
        # TO DO: include legend title features in theme
        # fluidRow(
        # h4("Title"),
        #  textInput("legendTitleText", label = h4("text"), value = "TO DO"),
        #  textInput("legendTitleColor", label = h4("color"), value = jsonTheme$fonts$legendTitle$color),
        #  sliderInput("legendTitleSize", label = h4("size"), min = 1, max = 30, value = jsonTheme$fonts$legendTitle$size, step = 0.5),
        #  sliderInput("legendTitleAngle", label = h4("angle"), min = -180, max = 180, value = jsonTheme$fonts$legendTitle$angle, step = 5)
        # ),
        tabsetPanel(
          tabPanel("Position",
                   selectInput("legendPosition", label = "position", choices = LegendPositions, selected = jsonTheme$background$legendPosition)
                   ),
          tabPanel("Font",
                   textInput("legendFontColor", label = "Color", value = jsonTheme$fonts$legend$color),
                   sliderInput("legendFontSize", label = "Size", min = 1, max = 30, value = jsonTheme$fonts$legend$size, step = 0.5),
                   sliderInput("legendFontAngle", label = "Angle", min = -180, max = 180, value = jsonTheme$fonts$legend$angle, step = 5)
                   ),
          tabPanel("Background",
                   textInput("legendFill", label = "Fill", value = jsonTheme$background$legend$fill),
                   textInput("legendColor", label = "Color", value = jsonTheme$background$legend$color),
                   sliderInput("legendSize", label = "Size", min = 0, max = 5, value = jsonTheme$background$legend$size, step = 0.05),
                   selectInput("legendLinetype", label = "Linetype", choices = Linetypes, selected = jsonTheme$background$legend$linetype)
                   )
          )),
      tabPanel(
        "Aesthetic Maps",
        tabsetPanel(
          tabPanel("Color",
                   sliderInput("colorMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$color), value = 1, step = 1),
                   uiOutput("colorMapValue")),
          tabPanel("Fill",
                   sliderInput("fillMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$fill), value = 1, step = 1),
                   uiOutput("fillMapValue")),
          tabPanel("Linetype",
                   sliderInput("linetypeMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$linetype), value = 1, step = 1),
                   uiOutput("linetypeMapValue")),
          tabPanel("Shape",
                   sliderInput("shapeMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$shape), value = 1, step = 1),
                   uiOutput("shapeMapValue")),
          tabPanel("Size",
                   sliderInput("sizeMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$size), value = 1, step = 1),
                   uiOutput("sizeMapValue")),
          tabPanel("Alpha",
                   sliderInput("alphaMapIndex", label = "Rank", min = 1, max = length(jsonTheme$aestheticMaps$alpha), value = 1, step = 1),
                   uiOutput("alphaMapValue"))
          )
      ),
      tabPanel(
        "Plot Configurations",
        tabsetPanel(
          tabPanel("addScatter",
                 p("WIP")),
          tabPanel("addLine",
                 p("WIP")),
          tabPanel("addRibbon",
                 p("WIP")),
          tabPanel("addErrorbar",
                 p("WIP")),
          tabPanel("plotBoxWhisker",
                   tabsetPanel(
                     tabPanel("Points",
                              tabsetPanel(
                                tabPanel("Color",
                                         selectInput("plotBoxWhiskerPointsColor", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$points$color),
                                         conditionalPanel("input.plotBoxWhiskerPointsColor=='other'",
                                                          textInput("plotBoxWhiskerPointsColor2", label = "", 
                                                                    value = jsonTheme$plotConfigurations$plotBoxWhisker$points$color))
                                         ),
                                tabPanel("Shape",
                                         selectInput("plotBoxWhiskerPointsShape", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$points$shape),
                                         conditionalPanel("input.plotBoxWhiskerPointsShape=='other'",
                                                          selectInput("plotBoxWhiskerPointsShape2", label = "", choices = Shapes, 
                                                                      selected = jsonTheme$plotConfigurations$plotBoxWhisker$points$shape))
                                         ),
                                tabPanel("Size",
                                         selectInput("plotBoxWhiskerPointsSize", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$points$size),
                                         conditionalPanel("input.plotBoxWhiskerPointsSize=='other'",
                                                          sliderInput("plotBoxWhiskerPointsSize2", label = "", min = 0, max = 5, 
                                                                      value = jsonTheme$plotConfigurations$plotBoxWhisker$points$size, step = 0.05))
                                         )
                                )
                              ),
                     tabPanel("Ribbons",
                              tabsetPanel(
                                tabPanel("Fill",
                                         selectInput("plotBoxWhiskerRibbonsFill", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$fill),
                                         conditionalPanel("input.plotBoxWhiskerRibbonsFill=='other'",
                                                          textInput("plotBoxWhiskerRibbonsFill2", label = "",
                                                                    value = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$fill))
                                         ),
                                tabPanel("Alpha",
                                         selectInput("plotBoxWhiskerRibbonsAlpha", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$alpha),
                                         conditionalPanel("input.plotBoxWhiskerRibbonsAlpha=='other'",
                                                          sliderInput("plotBoxWhiskerRibbonsAlpha2", label = "", min = 0, max = 1, 
                                                                      value = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$alpha, step = 0.05))
                                         ),
                                tabPanel("Color",
                                         selectInput("plotBoxWhiskerRibbonsColor", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$color),
                                         conditionalPanel("input.plotBoxWhiskerRibbonsColor=='other'",
                                                          textInput("plotBoxWhiskerRibbonsColor2", label = "",
                                                                    value = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$color))
                                         ),
                                tabPanel("Linetype",
                                         selectInput("plotBoxWhiskerRibbonsLinetype", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$linetype),
                                         conditionalPanel("input.plotBoxWhiskerRibbonsLinetype=='other'",
                                                          selectInput("plotBoxWhiskerRibbonsLinetype2", label = "", choices = Linetypes,
                                                                      selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$linetype))
                                         ),
                                tabPanel("Size",
                                         selectInput("plotBoxWhiskerRibbonsSize", 
                                                     label = "Selection key",
                                                     choices = c(other = "other", AestheticSelectionKeys),
                                                     selected = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$size),
                                         conditionalPanel("input.plotBoxWhiskerRibbonsSize=='other'",
                                                          sliderInput("plotBoxWhiskerRibbonsSize2", label = "", min = 0, max = 5, 
                                                                      value = jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$size, step = 0.05))
                                         )
                                )
                              )
                     )),
          tabPanel("plotDDIRatio",
                 p("WIP")),
          tabPanel("plotHistogram",
                 p("WIP")),
          tabPanel("plotObsVsPred",
                 p("WIP")),
          tabPanel("plotPKRatio",
                 p("WIP")),
          tabPanel("plotTimeProfile",
                 p("WIP")),
          tabPanel("plotTornado",
                 p("WIP"))
          )
      ))
  ),

  # Plot Column
  column(
    7,
    fluidRow(
      fileInput("loadTheme", "Load a Theme from Json", accept = ".json"),
      downloadButton("downloadJson", "Download theme .json")
      ),
      br(),
    fluidRow(
      selectInput(
        inputId = "selectedSamplePlot", label = "Sample Plot",
        choices = as.list(sapply(listOfAvailablePlots, identity)),
        selected = 1
      )
    ),
    br(),
    fluidRow(
      plotOutput(outputId = "samplePlot")
    )
  )
)

#---------- Server ----------#
server <- function(input, output) {
  
  #---------- Interactive way of updating aesthetic map element ----------#
  getColorMapValue <- reactive({jsonTheme$aestheticMaps$color[input$colorMapIndex]})
  getFillMapValue <- reactive({jsonTheme$aestheticMaps$fill[input$fillMapIndex]})
  getLinetypeMapValue <- reactive({jsonTheme$aestheticMaps$linetype[input$linetypeMapIndex]})
  getShapeMapValue <- reactive({jsonTheme$aestheticMaps$shape[input$shapeMapIndex]})
  getSizeMapValue <- reactive({jsonTheme$aestheticMaps$size[input$sizeMapIndex]})
  getAlphaMapValue <- reactive({jsonTheme$aestheticMaps$alpha[input$alphaMapIndex]})
  
  output$colorMapValue <- renderUI({textInput("colorMapValue2", "value", getColorMapValue())})
  output$fillMapValue <- renderUI({textInput("fillMapValue2", "value", getFillMapValue())})
  output$linetypeMapValue <- renderUI({selectInput("linetypeMapValue2", "value", choices = Linetypes, selected = getLinetypeMapValue())})
  output$shapeMapValue <- renderUI({selectInput("shapeMapValue2", "value", choices = Shapes, selected = getShapeMapValue())})
  output$sizeMapValue <- renderUI({numericInput("sizeMapValue2", "value", getSizeMapValue())})
  output$alphaMapValue <- renderUI({numericInput("alphaMapValue2", "value", getAlphaMapValue())})
  
  #---------- Interactive way of updating plot configurations  ----------#
  
  # Reactive function that updates theme R6 objects
  # The function uses expressions to prevent copy/paste of the code
  updateTheme <- reactive({
    #---------- Update Fonts ----------#
    # Each line will look like 'jsonTheme$fonts$title$color <- input$titleColor'
    plotProperties <- c("title", "subtitle", "xlabel", "ylabel", "watermark", "xAxis", "yAxis", "legend")
    fontProperties <- c("Color", "Size", "Angle")
    updateFontExpression <- parse(text = paste0(
      "jsonTheme$fonts$", rep(plotProperties, each = length(fontProperties)), 
      "$", rep(tolower(fontProperties), length(plotProperties)),
      " <- input$", rep(plotProperties, each = length(fontProperties)), rep(fontProperties, length(plotProperties))
    ))
    eval(updateFontExpression)
    
    # TO DO: account for legend title ?
    # jsonTheme$fonts$legendTitle$color <- input$legendTitleColor
    # jsonTheme$fonts$legendTitle$size <- input$legendTitleSize
    # jsonTheme$fonts$legendTitle$angle <- input$legendTitleAngle
    
    #---------- Update Background elements ----------#
    # Each line will look like 'jsonTheme$background$plot$color <- input$plotColor'
    plotProperties <- c("plot", "panel", "xAxis", "yAxis", "xGrid", "yGrid", "legend")
    fontProperties <- c("Color", "Size", "Linetype")
    updateBackgroundExpression <- parse(text = paste0(
      "jsonTheme$background$", rep(plotProperties, each = length(fontProperties)), 
      "$", rep(tolower(fontProperties), length(plotProperties)),
      " <- input$", rep(plotProperties, each = length(fontProperties)), rep(fontProperties, length(plotProperties))
    ))
    eval(updateBackgroundExpression)
    
    #---------- Update remaining fields not covered by expressions ----------#
    jsonTheme$background$legendPosition <- input$legendPosition
    jsonTheme$background$watermark <- input$watermarkText
    
    jsonTheme$background$plot$fill <- input$plotFill
    jsonTheme$background$panel$fill <- input$panelFill
    jsonTheme$background$legend$fill <- input$legendFill
    
    
    #---------- Aesthetic maps ----------#
    # Each line will look like 'if(length(input$colorMapValue2)>0){
    # jsonTheme$aestheticMaps$color[input$colorMapIndex] <- input$colorMapValue2}'
    mapProperties <- c("color", "fill", "linetype", "size", "alpha")
    updateMapExpression <- parse(text = paste0(
      "if(length(input$", mapProperties, "MapValue2)>0){jsonTheme$aestheticMaps$",
      mapProperties, "[input$", mapProperties, "MapIndex] <- input$", mapProperties, "MapValue2}"
    ))
    eval(updateMapExpression)
    # Update remaining field not covered by expressions (as.numeric enforced)
    if(length(input$shapeMapValue2)>0){
      jsonTheme$aestheticMaps$shape[input$shapeMapIndex] <- as.numeric(input$shapeMapValue2)}
    
    #---------- Plot configurations ----------#
    selectionProperties <- c("Color", "Fill", "Size", "Linetype", "Alpha")
    updateConfigurationExpression <- parse(text = paste0(
      "if(length(input$plotBoxWhiskerPoints", selectionProperties, ")>0){",
      "jsonTheme$plotConfigurations$plotBoxWhisker$ribbons$", tolower(selectionProperties), 
      " <- switch(input$plotBoxWhiskerRibbons", selectionProperties, ", ",
      "other = input$plotBoxWhiskerRibbons", selectionProperties, "2, ",
      "input$plotBoxWhiskerRibbons", selectionProperties, ")}"
      ))
    eval(updateConfigurationExpression)
    
    if(length(input$plotBoxWhiskerPointsSize)>0){
      jsonTheme$plotConfigurations$plotBoxWhisker$points$size <- switch(input$plotBoxWhiskerPointsSize,
                                                                        other = input$plotBoxWhiskerPointsSize2,
                                                                        input$plotBoxWhiskerPointsSize)}
    if(length(input$plotBoxWhiskerPointsColor)>0){
      jsonTheme$plotConfigurations$plotBoxWhisker$points$color <- switch(input$plotBoxWhiskerPointsColor,
                                                                         other = input$plotBoxWhiskerPointsColor2,
                                                                         input$plotBoxWhiskerPointsColor)}
    if(length(input$plotBoxWhiskerPointsShape)>0){
      jsonTheme$plotConfigurations$plotBoxWhisker$points$shape <- switch(input$plotBoxWhiskerPointsShape,
                                                                         other = as.numeric(input$plotBoxWhiskerPointsShape2),
                                                                         input$plotBoxWhiskerPointsShape)}
    
    #---------- Use theme for sample plots ----------#
    # This also set theme as current: after using app, current theme is changed
    useTheme(jsonTheme)
  })
  
  output$samplePlot <- renderPlot({
    
    # Reactive function that update theme object
    updateTheme()
    plotConfiguration <- PlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel")
    
    #---------- Define dataMapping from selected sample plot ----------#
    dataMapping <- switch(input$selectedSamplePlot,
                          initializePlot = NULL,
                          addScatter = XYGDataMapping$new(x = "Age", y = "Ratio", color = "Sex", shape = "Country"),
                          addLine = XYGDataMapping$new(x = "Age", y = "Ratio", color = "Sex", linetype = "Country"),
                          addRibbon = RangeDataMapping$new(x = "Age", ymin = "Obs", ymax = "Pred", fill = "Sex"),
                          addErrorbar = RangeDataMapping$new(x = "Age", ymin = "Obs", ymax = "Pred", color = "Sex", linetype = "Country"),
                          plotBoxWhisker = BoxWhiskerDataMapping$new(x = "AgeBin", y = "Ratio", fill = "Sex"),
                          plotDDIRatio = DDIRatioDataMapping$new(x = "Obs", y = "Pred", color = "Sex", shape = "Country"),
                          plotHistogram = HistogramDataMapping$new(x = "Ratio", fill = "Sex"),
                          plotObsVsPred = ObsVsPredDataMapping$new(x = "Obs", y = "Pred", color = "Sex", shape = "Country"),
                          plotPKRatio = PKRatioDataMapping$new(x = "Age", y = "Ratio", color = "Sex", shape = "Country"),
                          plotTimeProfile = TimeProfileDataMapping$new(x = "Age", y = "Ratio", ymin = "ymin", ymax = "ymax", fill = "Sex", color = "Sex", linetype = "Country"),
                          plotTornado = TornadoDataMapping$new(x = "Ratio", y = "ID")
                          )
    
    
    #---------- Define plotConfiguration from selected sample plot ----------#
    plotConfiguration <- switch(input$selectedSamplePlot,
                                initializePlot = plotConfiguration,
                                addScatter = plotConfiguration,
                                addLine = plotConfiguration,
                                addRibbon = plotConfiguration,
                                addErrorbar = plotConfiguration,
                                plotBoxWhisker = BoxWhiskerPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotDDIRatio = DDIRatioPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotHistogram = HistogramPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotObsVsPred = ObsVsPredPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotPKRatio = PKRatioPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotTimeProfile = TimeProfilePlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel"),
                                plotTornado = TornadoPlotConfiguration$new(title = "title", subtitle = "subtitle", xlabel = "xlabel", ylabel = "ylabel")
                                )
    
    #---------- Define plot from selected sample plot ----------#
    samplePlot <- switch(input$selectedSamplePlot,
                         initializePlot = initializePlot(plotConfiguration),
                         addScatter = addScatter(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         addLine = addLine(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         addRibbon = addRibbon(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         addErrorbar = addErrorbar(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotBoxWhisker = plotBoxWhisker(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotDDIRatio = plotDDIRatio(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotHistogram = plotHistogram(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotObsVsPred = plotObsVsPred(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotPKRatio = plotPKRatio(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         plotTimeProfile = plotObsVsPred(data = testData, dataMapping = dataMapping, plotConfiguration = plotConfiguration),
                         # use only 6 first values for the tornado plot
                         plotTornado = plotTornado(data = testData[1:6,], dataMapping = dataMapping, plotConfiguration = plotConfiguration)
                         )
    return(samplePlot)
  })  
  
  
  # Load/save Theme
  output$downloadJson <- downloadHandler(
    filename = "my-theme.json",
    content = function(file) {
      # Reactive function that update theme object
      updateTheme()
      saveThemeToJson(jsonFile = file, theme = jsonTheme)
    }
  )
  observeEvent(input$loadTheme, {
    jsonTheme <- loadThemeFromJson(input$loadTheme$datapath)
    useTheme(jsonTheme)
  })
}

shinyApp(ui = ui, server = server)
