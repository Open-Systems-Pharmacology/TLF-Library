require(shiny)
require(tlf)

jsonTheme <- jsonlite::fromJSON("tlf-theme.json")
linetypeList <- as.list(sapply(c("solid",
                                 "dashed",
                                 "dotted",
                                 "dotdash",
                                 "longdash",
                                 "twodash"), identity))

# Define UI page ----
ui <- fluidPage(
  h1("Theme Maker", align = "center"),
  column(
    6,
    tabsetPanel(
      tabPanel(
        "Labels",
        fluidRow(
          h4("Title:"),
          textInput("titleColor", label = "color", value = jsonTheme$labelColors$title),
          sliderInput("titleSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("Subtitle:"),
          textInput("subtitleColor", label = "color", value = jsonTheme$labelColors$subtitle),
          sliderInput("subtitleSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("X-label:"),
          textInput("xlabelColor", label = "color", value = jsonTheme$labelColors$xlabel),
          sliderInput("xlabelSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("Y-label:"),
          textInput("ylabelColor", label = "color", value = jsonTheme$labelColors$ylabel),
          sliderInput("ylabelSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        )
      ),
      tabPanel(
        "Watermark",
        fluidRow(
          textInput("watermarkText", label = h4("text"), value = jsonTheme$background$watermark),
          textInput("watermarkColor", label = h4("color"), value = jsonTheme$labelColors$watermark),
          sliderInput("watermarkSize", label = h4("size"), min = 1, max = 30, value = 12, step = 1)
        )
      ),
      tabPanel(
        "Background",
        fluidRow(
          h4("Background"),
          textInput("outerFill", label = h4("outer color"), value = jsonTheme$background$outer$fill),
          textInput("innerFill", label = h4("inner color"), value = jsonTheme$background$inner$fill)
        ),
        fluidRow(
          h4("Frame"),
          textInput("innerColor", label = h4("color"), value = jsonTheme$background$inner$color),
          sliderInput("innerSize", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$background$inner$size, step = 0.05),
          selectInput("innerLinetype", label = h4("linetype"), choices = linetypeList, selected = jsonTheme$background$inner$linetype)
        ),
        fluidRow(
          h4("Grid"),
          textInput("gridColor", label = h4("color"), value = jsonTheme$background$grid$color),
          sliderInput("gridSize", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$background$grid$size, step = 0.05),
          selectInput("gridLinetype", label = h4("linetype"), choices = linetypeList, selected = jsonTheme$background$grid$linetype)
        )
      ),
      tabPanel(
        "Aesthetics",
        fluidRow(
          h4("...")
        )
      ),
      tabPanel(
        "Options",
        conditionalPanel(
          condition = "input.plotType == 'plotPKRatio()'",
          h4("PK Ratio Line of unity"),
          textInput("pkRatioLine1.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[1]),
          selectInput("pkRatioLine1.linetype", label = h4("linetype"), choices = linetypeList, selected = jsonTheme$defaultCaption$pkRatio$linetype[1]),
          sliderInput("pkRatioLine1.size", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$defaultCaption$pkRatio$size[1], step = 0.05),
          h4("PK Ratio Lines of 1.5 fold error"),
          textInput("pkRatioLine2.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[2]),
          selectInput("pkRatioLine2.linetype", label = h4("linetype"), choices = linetypeList, selected = jsonTheme$defaultCaption$pkRatio$linetype[2]),
          sliderInput("pkRatioLine2.size", label = h4("size"), min = 0.05, max = 5, value = jsonTheme$defaultCaption$pkRatio$size[2], step = 0.05),
          h4("PK Ratio Lines of 2 fold error"),
          textInput("pkRatioLine3.color", label = h4("color"), value = jsonTheme$defaultCaption$pkRatio$color[3]),
          selectInput("pkRatioLine3.linetype", label = h4("linetype"), choices = linetypeList, selected = jsonTheme$defaultCaption$pkRatio$linetype[3]),
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
    6,
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

    actionButton("exportButton", "Export Theme object"),
    actionButton("setCurrentThemeButton", label = "Set as current theme")
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  output$samplePlot <- renderPlot({
    # Create copy of json template
    updatedThemeProperties <- jsonTheme
    # Update every template feature
    updatedThemeProperties$labelColors$title <- input$titleColor
    updatedThemeProperties$labelColors$subtitle <- input$subtitleColor
    updatedThemeProperties$labelColors$xlabel <- input$xlabelColor
    updatedThemeProperties$labelColors$ylabel <- input$ylabelColor

    updatedThemeProperties$background$outer$fill <- input$outerFill
    updatedThemeProperties$background$inner$fill <- input$innerFill
    updatedThemeProperties$background$inner$color <- input$innerColor
    updatedThemeProperties$background$grid$color <- input$gridColor

    updatedThemeProperties$background$inner$size <- input$innerSize
    updatedThemeProperties$background$grid$size <- input$gridSize
    updatedThemeProperties$background$inner$linetype <- input$innerLinetype
    updatedThemeProperties$background$grid$linetype <- input$gridLinetype

    # Create Theme object
    watermark <- Label$new(
      text = input$watermarkText,
      size = input$watermarkSize,
      color = input$watermarkColor
    )
    updatedTheme <- Theme$new(
      themesProperties = updatedThemeProperties,
      watermark = watermark
    )
    # Update some Theme properties
    updatedTheme$titleFont$size <- input$titleSize
    updatedTheme$subtitleFont$size <- input$subtitleSize
    # TO DO: Find and fix issue
    # updatedTheme$xlabelFont$size <- input$xlabelSize
    # updatedTheme$ylabelFont$size <- input$ylabelSize

    plotConfiguration <- PlotConfiguration$new(
      title = "Title",
      subtitle = "Subtitle",
      xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
      ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
      theme = updatedTheme
    )

    x <- seq(-2, 2, 0.1)
    y <- cos(x)
    sampleData <- data.frame(x = x, y = y, ymax = y, ymin = 0)

    samplePlot <- initializePlot(plotConfiguration = plotConfiguration)

    if (input$plotType %in% "addScatter()") {
      samplePlot <- addScatter(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "addLine()") {
      samplePlot <- addLine(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "addRibbon()") {
      samplePlot <- addRibbon(data = sampleData, plotConfiguration = plotConfiguration)
    }
    if (input$plotType %in% "addErrorbar()") {
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
      plotConfiguration <- TornadoPlotConfiguration$new(title = "Title",
                                                        subtitle = "Subtitle",
                                                        xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
                                                        ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
                                                        theme = updatedTheme,
                                                        colorPalette = input$colorPalette)
      samplePlot <- plotTornado(
        x = c(1, 0.5, -0.25, -0.2, -0.1),
        y = c("A", "B", "C", "D", "E"),
        plotConfiguration = plotConfiguration
      )
    }
    samplePlot
  })
}

shinyApp(ui = ui, server = server)
