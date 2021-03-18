require(shiny)
require(tlf)

jsonTheme <- loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf"))
useTheme(jsonTheme)

# Define UI page ----
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
                     textInput("titleColor", label = "color", value = jsonTheme$fonts$title$color),
                     sliderInput("titleSize", label = "size", min = 1, max = 30, value = jsonTheme$fonts$title$size, step = 0.5),
                     sliderInput("titleAngle", label = "angle", min = -180, max = 180, value = jsonTheme$fonts$title$angle, step = 5)
                   )),
          tabPanel("Subtitle",
                   fluidRow(
                     textInput("subtitleColor", label = "color", value = jsonTheme$fonts$subtitle$color),
                     sliderInput("subtitleSize", label = "size", min = 1, max = 30, value = jsonTheme$fonts$subtitle$size, step = 0.5),
                     sliderInput("subtitleAngle", label = "angle", min = -180, max = 180, value = jsonTheme$fonts$subtitle$angle, step = 5)
                   )),
          tabPanel("X-label",
                   fluidRow(
                     textInput("xlabelColor", label = "color", value = jsonTheme$fonts$xlabel$color),
                     sliderInput("xlabelSize", label = "size", min = 1, max = 30, value = jsonTheme$fonts$xlabel$size, step = 0.5),
                     sliderInput("xlabelAngle", label = "angle", min = -180, max = 180, value = jsonTheme$fonts$xlabel$angle, step = 5)
                   )),
          tabPanel("Y-label",
                   fluidRow(
                     textInput("ylabelColor", label = "color", value = jsonTheme$fonts$ylabel$color),
                     sliderInput("ylabelSize", label = "size", min = 1, max = 30, value = jsonTheme$fonts$ylabel$size, step = 0.5),
                     sliderInput("ylabelAngle", label = "angle", min = -180, max = 180, value = jsonTheme$fonts$ylabel$angle, step = 5)
                   ))
        )),
      tabPanel(
        "Watermark",
        fluidRow(
          textInput("watermarkText", label = h4("text"), value = jsonTheme$background$watermark),
          textInput("watermarkColor", label = h4("color"), value = jsonTheme$fonts$watermark$color),
          sliderInput("watermarkSize", label = h4("size"), min = 1, max = 30, value = jsonTheme$fonts$watermark$size, step = 0.5),
          sliderInput("watermarkAngle", label = h4("angle"), min = -180, max = 180, value = jsonTheme$fonts$watermark$angle, step = 5)
        )
      ),
      tabPanel(
        "Background",
        tabsetPanel(
          tabPanel("Plot Area",
                   textInput("plotFill", label = h4("plot area fill"), value = jsonTheme$background$plot$fill),
          textInput("plotColor", label = h4("plot area frame color"), value = jsonTheme$background$plot$color),
          sliderInput("plotSize", label = h4("plot area frame size"), min = 0, max = 5, value = jsonTheme$background$plot$size, step = 0.05),
          selectInput("plotLinetype", label = h4("plot area frame linetype"), choices = Linetypes, selected = jsonTheme$background$plot$linetype)
        ),
        tabPanel("Panel Area",
          textInput("panelFill", label = h4("panel area fill"), value = jsonTheme$background$panel$fill),
          textInput("panelColor", label = h4("panel area frame color"), value = jsonTheme$background$panel$color),
          sliderInput("panelSize", label = h4("panel area frame size"), min = 0, max = 5, value = jsonTheme$background$panel$size, step = 0.05),
          selectInput("panelLinetype", label = h4("panel area frame linetype"), choices = Linetypes, selected = jsonTheme$background$panel$linetype)
        )
      )),
      tabPanel(
        "Axes",
        tabsetPanel(
          tabPanel("X-axis",
          textInput("xAxisColor", label = h4("axis color"), value = jsonTheme$background$xAxis$color),
          sliderInput("xAxisSize", label = h4("axis size"), min = 0, max = 5, value = jsonTheme$background$xAxis$size, step = 0.05),
          selectInput("xAxisLinetype", label = h4("axis linetype"), choices = Linetypes, selected = jsonTheme$background$xAxis$linetype),
          textInput("xAxisTicksColor", label = h4("ticklabels color"), value = jsonTheme$fonts$xAxis$color),
          sliderInput("xAxisTicksSize", label = h4("ticklabels size"), min = 0, max = 30, value = jsonTheme$fonts$xAxis$size, step = 0.5),
          sliderInput("xAxisTicksAngle", label = h4("ticklabels angle"), min = -180, max = 180, value = jsonTheme$fonts$xAxis$angle, step = 5)
        ),
        tabPanel("Y-axis",
          textInput("yAxisColor", label = h4("y-axis color"), value = jsonTheme$background$yAxis$color),
          sliderInput("yAxisSize", label = h4("y-axis size"), min = 0, max = 5, value = jsonTheme$background$yAxis$size, step = 0.05),
          selectInput("yAxisLinetype", label = h4("y-axis linetype"), choices = Linetypes, selected = jsonTheme$background$yAxis$linetype),
          textInput("yAxisTicksColor", label = h4("ticklabels color"), value = jsonTheme$fonts$xAxis$color),
          sliderInput("yAxisTicksSize", label = h4("ticklabels size"), min = 0, max = 30, value = jsonTheme$fonts$yAxis$size, step = 0.5),
          sliderInput("yAxisTicksAngle", label = h4("ticklabels angle"), min = -180, max = 180, value = jsonTheme$fonts$yAxis$angle, step = 5)
        )
      )),
      tabPanel(
        "Grid",
        tabsetPanel(
          tabPanel("X-grid",
          textInput("xGridColor", label = h4("color"), value = jsonTheme$background$xGrid$color),
          sliderInput("xGridSize", label = h4("size"), min = 0, max = 5, value = jsonTheme$background$xGrid$size, step = 0.05),
          selectInput("xGridLinetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$background$xGrid$linetype)
        ),
        tabPanel("Y-Grid",
          textInput("yGridColor", label = h4("color"), value = jsonTheme$background$yGrid$color),
          sliderInput("yGridSize", label = h4("size"), min = 0, max = 5, value = jsonTheme$background$yGrid$size, step = 0.05),
          selectInput("yGridLinetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$background$yGrid$linetype)
        )
      )),
      tabPanel(
        "Legend",
        # fluidRow(
        # h4("Title"),
        #  textInput("legendTitleText", label = h4("text"), value = "TO DO"),
        #  textInput("legendTitleColor", label = h4("color"), value = jsonTheme$fonts$legendTitle$color),
        #  sliderInput("legendTitleSize", label = h4("size"), min = 1, max = 30, value = jsonTheme$fonts$legendTitle$size, step = 0.5),
        #  sliderInput("legendTitleAngle", label = h4("angle"), min = -180, max = 180, value = jsonTheme$fonts$legendTitle$angle, step = 5)
        # ),
        tabsetPanel(
          tabPanel("Position",
          selectInput("legendPosition", label = h4("position"), choices = LegendPositions, selected = jsonTheme$background$legendPosition)
        ),
        tabPanel("Font",
          textInput("legendFontColor", label = h4("color"), value = jsonTheme$fonts$legend$color),
          sliderInput("legendFontSize", label = h4("size"), min = 1, max = 30, value = jsonTheme$fonts$legend$size, step = 0.5),
          sliderInput("legendFontAngle", label = h4("angle"), min = -180, max = 180, value = jsonTheme$fonts$legend$angle, step = 5)
        ),
        tabPanel("Background",
          textInput("legendFill", label = h4("fill"), value = jsonTheme$background$legend$fill),
          textInput("legendColor", label = h4("color"), value = jsonTheme$background$legend$color),
          sliderInput("legendSize", label = h4("size"), min = 0, max = 5, value = jsonTheme$background$legend$size, step = 0.05),
          selectInput("legendLinetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$background$legend$linetype)
        )
      )),
      tabPanel(
        "Aesthetic Maps",
        tabsetPanel(
          tabPanel("Color",
          sliderInput("colorMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$color), value = 1, step = 1),
          uiOutput("colorMapValue")),
          tabPanel("Fill",
          sliderInput("fillMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$fill), value = 1, step = 1),
          uiOutput("fillMapValue")),
          tabPanel("Linetype",
          sliderInput("linetypeMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$linetype), value = 1, step = 1),
          uiOutput("linetypeMapValue")),
          tabPanel("Shape",
          sliderInput("shapeMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$shape), value = 1, step = 1),
          uiOutput("shapeMapValue")),
          tabPanel("Size",
          sliderInput("sizeMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$size), value = 1, step = 1),
          uiOutput("sizeMapValue")),
          tabPanel("Alpha",
          sliderInput("alphaMapIndex", label = h4("rank"), min = 1, max = length(jsonTheme$aestheticMaps$alpha), value = 1, step = 1),
          uiOutput("alphaMapValue"))
          )
      ),
      tabPanel(
        "Plot Configurations",
        p("The following arguments define how the plot will use the aesthetic map"),
        p("Selection key 'next': use next aesthetic value from map"),
        p("Selection key 'same': use same aesthetic value from map"),
        p("Selection key 'first': use first aesthetic value from map"),
        p("Selection key 'reset': use next aesthetic value from map, but reset every time the function is called"),
        p("No selection key, the specific value is used every time (e.g. 'blank')"),
        conditionalPanel(
          condition = "input.plotType == 'addScatter()'",
          textInput("addScatterColor", label = h4("color"), value = jsonTheme$plotConfigurations$addScatter$color),
          textInput("addScatterLinetype", label = h4("linetype"), value = jsonTheme$plotConfigurations$addScatter$linetype),
          textInput("addScatterShape", label = h4("shape"), value = jsonTheme$plotConfigurations$addScatter$shape),
          textInput("addScatterSize", label = h4("size"), value = jsonTheme$plotConfigurations$addScatter$size),
          textInput("addScatterFill", label = h4("fill"), value = jsonTheme$plotConfigurations$addScatter$fill),
          textInput("addScatterAlpha", label = h4("alpha"), value = jsonTheme$plotConfigurations$addScatter$alpha)
        ),
        conditionalPanel(
          condition = "input.plotType == 'addLine()'",
          textInput("addLineColor", label = h4("color"), value = jsonTheme$plotConfigurations$addLine$color),
          textInput("addLineLinetype", label = h4("linetype"), value = jsonTheme$plotConfigurations$addLine$linetype),
          textInput("addLineShape", label = h4("shape"), value = jsonTheme$plotConfigurations$addLine$shape),
          textInput("addLineSize", label = h4("size"), value = jsonTheme$plotConfigurations$addLine$size),
          textInput("addLineFill", label = h4("fill"), value = jsonTheme$plotConfigurations$addLine$fill),
          textInput("addLineAlpha", label = h4("alpha"), value = jsonTheme$plotConfigurations$addLine$alpha)
        ),
        conditionalPanel(
          condition = "input.plotType == 'addRibbon()'",
          textInput("addRibbonColor", label = h4("color"), value = jsonTheme$plotConfigurations$addRibbon$color),
          textInput("addRibbonLinetype", label = h4("linetype"), value = jsonTheme$plotConfigurations$addRibbon$linetype),
          textInput("addRibbonShape", label = h4("shape"), value = jsonTheme$plotConfigurations$addRibbon$shape),
          textInput("addRibbonSize", label = h4("size"), value = jsonTheme$plotConfigurations$addRibbon$size),
          textInput("addRibbonFill", label = h4("fill"), value = jsonTheme$plotConfigurations$addRibbon$fill),
          textInput("addRibbonAlpha", label = h4("alpha"), value = jsonTheme$plotConfigurations$addRibbon$alpha)
        ),
        conditionalPanel(
          condition = "input.plotType == 'addErrorbar()'",
          textInput("addErrorbarColor", label = h4("color"), value = jsonTheme$plotConfigurations$addErrorbar$color),
          textInput("addErrorbarLinetype", label = h4("linetype"), value = jsonTheme$plotConfigurations$addErrorbar$linetype),
          textInput("addErrorbarShape", label = h4("shape"), value = jsonTheme$plotConfigurations$addErrorbar$shape),
          textInput("addErrorbarSize", label = h4("size"), value = jsonTheme$plotConfigurations$addErrorbar$size),
          textInput("addErrorbarFill", label = h4("fill"), value = jsonTheme$plotConfigurations$addErrorbar$fill),
          textInput("addErrorbarAlpha", label = h4("alpha"), value = jsonTheme$plotConfigurations$addErrorbar$alpha)
        ),
        conditionalPanel(
          condition = "input.plotType == 'plotPKRatio()'",
          h4("PK Ratio Line of unity"),
          textInput("pkRatioLine1.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[1]),
          selectInput("pkRatioLine1.linetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$defaultCaption$pkRatio$linetype[1]),
          sliderInput("pkRatioLine1.size", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$defaultCaption$pkRatio$size[1], step = 0.05),
          h4("PK Ratio Lines of 1.5 fold error"),
          textInput("pkRatioLine2.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[2]),
          selectInput("pkRatioLine2.linetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$defaultCaption$pkRatio$linetype[2]),
          sliderInput("pkRatioLine2.size", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$defaultCaption$pkRatio$size[2], step = 0.05),
          h4("PK Ratio Lines of 2 fold error"),
          textInput("pkRatioLine3.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[3]),
          selectInput("pkRatioLine3.linetype", label = h4("linetype"), choices = Linetypes, selected = jsonTheme$defaultCaption$pkRatio$linetype[3]),
          sliderInput("pkRatioLine3.size", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$defaultCaption$pkRatio$size[3], step = 0.05)
        ),
        conditionalPanel(
          condition = "input.plotType == 'plotTornado()'",
          textInput("colorPalette", label = h4("Color Palette"), value = NULL)
        )
      )
    )
  ),

  # Plot Column
  column(
    7,
    h2("Sample Plot", align = "center"),

    plotOutput(outputId = "samplePlot"),

    selectInput(
      inputId = "plotType", label = h3("Available plot functions"),
      choices = as.list(sapply(
        c(
          "initializePlot()",
          "addScatter()",
          "addLine()",
          "addRibbon()",
          "addErrorbar()",
          "plotPKRatio()",
          "plotDDIRatio()",
          "plotBoxWhisker()",
          "plotHistogram()",
          "plotTimeProfile()",
          "plotTornado()",
          "plotObsVsPred()"
        ),
        identity
      )),
      selected = 1
    ),

    fileInput("loadTheme", "Load a Theme from Json", accept = ".json"),
    textInput("jsonFileName", label = "File name", value = "new-theme.json"),
    actionButton("saveTheme", "Save Theme to Json")
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  output$samplePlot <- renderPlot({
    # Create copy of json template
    updatedThemeProperties <- jsonTheme
    # Update every template feature
    jsonTheme$fonts$title$color <- input$titleColor
    jsonTheme$fonts$subtitle$color <- input$subtitleColor
    jsonTheme$fonts$xlabel$color <- input$xlabelColor
    jsonTheme$fonts$ylabel$color <- input$ylabelColor

    jsonTheme$fonts$title$size <- input$titleSize
    jsonTheme$fonts$subtitle$size <- input$subtitleSize
    jsonTheme$fonts$xlabel$size <- input$xlabelSize
    jsonTheme$fonts$ylabel$size <- input$ylabelSize

    jsonTheme$fonts$title$angle <- input$titleAngle
    jsonTheme$fonts$subtitle$angle <- input$subtitleAngle
    jsonTheme$fonts$xlabel$angle <- input$xlabelAngle
    jsonTheme$fonts$ylabel$angle <- input$ylabelAngle

    jsonTheme$background$watermark <- input$watermarkText
    jsonTheme$fonts$watermark$color <- input$watermarkColor
    jsonTheme$fonts$watermark$size <- input$watermarkSize
    jsonTheme$fonts$watermark$angle <- input$watermarkAngle

    jsonTheme$background$plot$fill <- input$plotFill
    jsonTheme$background$plot$color <- input$plotColor
    jsonTheme$background$plot$size <- input$plotSize
    jsonTheme$background$plot$linetype <- input$plotLinetype

    jsonTheme$background$panel$fill <- input$panelFill
    jsonTheme$background$panel$color <- input$panelColor
    jsonTheme$background$panel$size <- input$panelSize
    jsonTheme$background$panel$linetype <- input$panelLinetype

    jsonTheme$background$xAxis$color <- input$xAxisColor
    jsonTheme$background$xAxis$size <- input$xAxisSize
    jsonTheme$background$xAxis$linetype <- input$xAxisLinetype
    jsonTheme$fonts$xAxis$color <- input$xAxisTicksColor
    jsonTheme$fonts$xAxis$size <- input$xAxisTicksSize
    jsonTheme$fonts$xAxis$angle <- input$xAxisTicksAngle

    jsonTheme$background$yAxis$color <- input$yAxisColor
    jsonTheme$background$yAxis$size <- input$yAxisSize
    jsonTheme$background$yAxis$linetype <- input$yAxisLinetype
    jsonTheme$fonts$yAxis$color <- input$yAxisTicksColor
    jsonTheme$fonts$yAxis$size <- input$yAxisTicksSize
    jsonTheme$fonts$yAxis$angle <- input$yAxisTicksAngle

    jsonTheme$background$xGrid$color <- input$xGridColor
    jsonTheme$background$xGrid$size <- input$xGridSize
    jsonTheme$background$xGrid$linetype <- input$xGridLinetype

    jsonTheme$background$yGrid$color <- input$yGridColor
    jsonTheme$background$yGrid$size <- input$yGridSize
    jsonTheme$background$yGrid$linetype <- input$yGridLinetype

    jsonTheme$background$legendPosition <- input$legendPosition

    jsonTheme$background$legend$fill <- input$legendFill
    jsonTheme$background$legend$color <- input$legendColor
    jsonTheme$background$legend$size <- input$legendSize
    jsonTheme$background$legend$linetype <- input$legendLinetype

    jsonTheme$fonts$legend$color <- input$legendFontColor
    jsonTheme$fonts$legend$size <- input$legendFontSize
    jsonTheme$fonts$legend$angle <- input$legendFontAngle

    # jsonTheme$fonts$legendTitle$color <- input$legendTitleColor
    # jsonTheme$fonts$legendTitle$size <- input$legendTitleSize
    # jsonTheme$fonts$legendTitle$angle <- input$legendTitleAngle
    
    # Color maps
    if(!isOfLength(input$colorMapValue2,0)){jsonTheme$aestheticMaps$color[input$colorMapIndex] <- input$colorMapValue2}
    if(!isOfLength(input$fillMapValue2,0)){jsonTheme$aestheticMaps$fill[input$fillMapIndex] <- input$fillMapValue2}
    if(!isOfLength(input$linetypeMapValue2,0)){jsonTheme$aestheticMaps$linetype[input$linetypeMapIndex] <- input$linetypeMapValue2}
    if(!isOfLength(input$shapeMapValue2,0)){jsonTheme$aestheticMaps$shape[input$shapeMapIndex] <- as.numeric(input$shapeMapValue2)}
    if(!isOfLength(input$sizeMapValue2,0)){jsonTheme$aestheticMaps$size[input$sizeMapIndex] <- input$sizeMapValue2}
    if(!isOfLength(input$alphaMapValue2,0)){jsonTheme$aestheticMaps$alpha[input$alphaMapIndex] <- input$alphaMapValue2}

    # initializePlot() from Theme
    useTheme(jsonTheme)
    plotConfiguration <- PlotConfiguration$new(title = "Title", subtitle = "Subtitle", xlabel = "X-label", ylabel = "Y-label")
    samplePlot <- initializePlot(plotConfiguration = plotConfiguration)

    # addScatter() from Theme
    if (input$plotType %in% "addScatter()") {
      jsonTheme$plotConfigurations$addScatter$color <- input$addScatterColor
      jsonTheme$plotConfigurations$addScatter$linetype <- input$addScatterLinetype
      jsonTheme$plotConfigurations$addScatter$shape <- input$addScatterShape
      jsonTheme$plotConfigurations$addScatter$size <- input$addScatterSize
      jsonTheme$plotConfigurations$addScatter$fill <- input$addScatterFill
      jsonTheme$plotConfigurations$addScatter$alpha <- input$addScatterAlpha
      useTheme(jsonTheme)
      plotConfiguration <- PlotConfiguration$new(title = "Title", subtitle = "Subtitle", xlabel = "X-label", ylabel = "Y-label")

      sampleData <- data.frame(
        x = seq(-3, 3, 0.1),
        y = cos(2 * seq(-3, 3, 0.1))
      )
      samplePlot <- addScatter(data = sampleData, plotConfiguration = plotConfiguration)
      for (sampleIndex in 1:4) {
        sampleData$y <- 1.2 * sampleData$y
        samplePlot <- addScatter(data = sampleData, plotObject = samplePlot)
      }
    }
    if (input$plotType %in% "addLine()") {
      jsonTheme$plotConfigurations$addLine$color <- input$addLineColor
      jsonTheme$plotConfigurations$addLine$linetype <- input$addLineLinetype
      jsonTheme$plotConfigurations$addLine$shape <- input$addLineShape
      jsonTheme$plotConfigurations$addLine$size <- input$addLineSize
      jsonTheme$plotConfigurations$addLine$fill <- input$addLineFill
      jsonTheme$plotConfigurations$addLine$alpha <- input$addLineAlpha
      useTheme(jsonTheme)
      plotConfiguration <- PlotConfiguration$new(title = "Title", subtitle = "Subtitle", xlabel = "X-label", ylabel = "Y-label")

      sampleData <- data.frame(
        x = seq(-3, 3, 0.1),
        y = cos(2 * seq(-3, 3, 0.1))
      )
      samplePlot <- addLine(data = sampleData, plotConfiguration = plotConfiguration)
      for (sampleIndex in 1:4) {
        sampleData$y <- 1.2 * sampleData$y
        samplePlot <- addLine(data = sampleData, plotObject = samplePlot)
      }
    }
    if (input$plotType %in% "addRibbon()") {
      jsonTheme$plotConfigurations$addRibbon$color <- input$addRibbonColor
      jsonTheme$plotConfigurations$addRibbon$linetype <- input$addRibbonLinetype
      jsonTheme$plotConfigurations$addRibbon$shape <- input$addRibbonShape
      jsonTheme$plotConfigurations$addRibbon$size <- input$addRibbonSize
      jsonTheme$plotConfigurations$addRibbon$fill <- input$addRibbonFill
      jsonTheme$plotConfigurations$addRibbon$alpha <- input$addRibbonAlpha
      useTheme(jsonTheme)
      plotConfiguration <- PlotConfiguration$new(title = "Title", subtitle = "Subtitle", xlabel = "X-label", ylabel = "Y-label")

      sampleData <- data.frame(
        x = seq(-3, 3, 0.1),
        ymax = cos(2 * seq(-3, 3, 0.1)),
        ymin = 0
      )
      samplePlot <- addRibbon(data = sampleData, plotConfiguration = plotConfiguration)
      for (sampleIndex in 1:4) {
        sampleData$ymin <- sampleData$ymax
        sampleData$ymax <- 1.2 * sampleData$ymax
        samplePlot <- addRibbon(data = sampleData, plotObject = samplePlot)
      }
    }
    if (input$plotType %in% "addErrorbar()") {
      jsonTheme$plotConfigurations$addErrorbar$color <- input$addErrorbarColor
      jsonTheme$plotConfigurations$addErrorbar$linetype <- input$addErrorbarLinetype
      jsonTheme$plotConfigurations$addErrorbar$shape <- input$addErrorbarShape
      jsonTheme$plotConfigurations$addErrorbar$size <- input$addErrorbarSize
      jsonTheme$plotConfigurations$addErrorbar$fill <- input$addErrorbarFill
      jsonTheme$plotConfigurations$addErrorbar$alpha <- input$addErrorbarAlpha
      useTheme(jsonTheme)
      
      plotConfiguration <- PlotConfiguration$new(title = "Title", subtitle = "Subtitle", xlabel = "X-label", ylabel = "Y-label")
      
      sampleData <- data.frame(
        x = seq(-3, 3, 0.1),
        ymax = cos(2 * seq(-3, 3, 0.1)),
        ymin = 0
      )
      samplePlot <- addErrorbar(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "plotPKRatio()") {
      pkRatioCaption <- data.frame(
        name = c("pkRatioLine1", "pkRatioLine2", "pkRatioLine3"),
        label = c("pkRatioLine1", "pkRatioLine2", "pkRatioLine3"),
        visibility = rep(FALSE, 3), order = c(1, 2, 3),
        color = c(input$pkRatioLine1.color, input$pkRatioLine2.color, input$pkRatioLine3.color),
        shape = rep(-2, 3),
        size = c(input$pkRatioLine1.size, input$pkRatioLine2.size, input$pkRatioLine3.size),
        linetype = c(input$pkRatioLine1.linetype, input$pkRatioLine2.linetype, input$pkRatioLine3.linetype),
        fill = NA,
        stringsAsFactors = FALSE
      )

      plotConfiguration <- PKRatioPlotConfiguration$new(
        pkRatioCaption = pkRatioCaption,
        title = "Title",
        subtitle = "Subtitle",
        xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
        ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
        theme = updatedTheme
      )
      samplePlot <- plotPKRatio(
        data = sampleData,
        plotConfiguration = plotConfiguration
      )
    }
    if (input$plotType %in% "plotDDIRatio()") {
      plotConfiguration <- DDIRatioPlotConfiguration$new(
        title = "Title",
        subtitle = "Subtitle",
        xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
        ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
        theme = updatedTheme
      )
      samplePlot <- plotDDIRatio(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "plotBoxWhisker()") {
      # plotConfiguration <- BoxWhiskerPlotConfiguration$new(
      # samplePlot <- plotBoxWhisker(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "plotHistogram()") {
      # plotConfiguration <- HistogramPlotConfiguration$new(
      # samplePlot <- plotHistogram(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "plotTimeProfile()") {
      # plotConfiguration <- TimeProfilePlotConfiguration$new(
      # samplePlot <- plotTimeProfile(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "plotObsVsPred()") {
      # plotConfiguration <- ObsVsPredPlotConfiguration$new(
      # samplePlot <- plotBoxWhisker(data = sampleData, plotConfiguration = plotConfiguration)
    }

    if (input$plotType %in% "plotTornado()") {
      plotConfiguration <- TornadoPlotConfiguration$new(
        title = "Title",
        subtitle = "Subtitle",
        xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
        ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
        theme = updatedTheme,
        colorPalette = input$colorPalette
      )
      samplePlot <- plotTornado(
        x = c(1, 0.5, -0.25, -0.2, -0.1),
        y = c("A", "B", "C", "D", "E"),
        plotConfiguration = plotConfiguration
      )
    }
    samplePlot
  })
  
  # Interactive way of updating aesthetic map element
  getColorMapValue <- reactive({jsonTheme$aestheticMaps$color[input$colorMapIndex]})
  getFillMapValue <- reactive({jsonTheme$aestheticMaps$fill[input$fillMapIndex]})
  getLinetypeMapValue <- reactive({jsonTheme$aestheticMaps$linetype[input$linetypeMapIndex]})
  getShapeMapValue <- reactive({jsonTheme$aestheticMaps$shape[input$shapeMapIndex]})
  getSizeMapValue <- reactive({jsonTheme$aestheticMaps$size[input$sizeMapIndex]})
  getAlphaMapValue <- reactive({jsonTheme$aestheticMaps$alpha[input$alphaMapIndex]})
  
  output$colorMapValue <- renderUI({textInput("colorMapValue2", "value", getColorMapValue())})
  output$fillMapValue <- renderUI({textInput("fillMapValue2", "value", getFillMapValue())})
  output$linetypeMapValue <- renderUI({
    selectInput("linetypeMapValue2", "value", choices = Linetypes, selected = getLinetypeMapValue())})
  output$shapeMapValue <- renderUI({
    selectInput("shapeMapValue2", "value", choices = Shapes, selected = getShapeMapValue())})
  output$sizeMapValue <- renderUI({numericInput("sizeMapValue2", "value", getSizeMapValue())})
  output$alphaMapValue <- renderUI({numericInput("alphaMapValue2", "value", getAlphaMapValue())})
  
  # Load/save Theme
  observeEvent(input$loadTheme, {
    jsonTheme <- loadThemeFromJson(input$loadTheme$datapath)
    useTheme(jsonTheme)
  })
  observeEvent(input$saveTheme, {saveThemeToJson(jsonFile = input$jsonFileName, theme = jsonTheme)})
}

shinyApp(ui = ui, server = server)
