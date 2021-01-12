require(shiny)
require(tlf)

jsonTheme <- jsonlite::fromJSON("tlf-theme.json")

# Define UI page ----
ui <- fluidPage(
  h1("Theme features", align = "center"),
  column(
    6,
    tabsetPanel(
      tabPanel(
        "Labels",
        fluidRow(
          h4("Title:"),
          textInput("titleColor", label = "color", value = "black"),
          sliderInput("titleSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("Subtitle:"),
          textInput("subtitleColor", label = "color", value = "black"),
          sliderInput("subtitleSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("X-label:"),
          textInput("xlabelColor", label = "color", value = "black"),
          sliderInput("xlabelSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        ),
        fluidRow(
          h4("Y-label:"),
          textInput("ylabelColor", label = "color", value = "black"),
          sliderInput("ylabelSize", label = "size", min = 1, max = 30, value = 12, step = 1)
        )
      ),
      tabPanel(
        "Watermark",
        fluidRow(
          textInput("watermarkText", label = h4("text"), value = "Not QC'ed !"),
          textInput("watermarkColor", label = h4("color"), value = "black"),
          sliderInput("watermarkSize", label = h4("size"), min = 1, max = 30, value = 12, step = 1)
        )
      ),
      tabPanel(
        "Background",
        fluidRow(
          h4("Background"),
          textInput("outerFill", label = h4("outer color"), value = "white"),
          textInput("innerFill", label = h4("inner color"), value = "white")
        ),
        fluidRow(
          h4("Frame"),
          textInput("innerColor", label = h4("color"), value = "black"),
          sliderInput("innerSize", label = h4("size"), min = 0.1, max = 5, value = 1, step = 0.1),
          textInput("innerLinetype", label = h4("linetype"), value = "solid")
        ),
        fluidRow(
          h4("Grid"),
          textInput("gridColor", label = h4("color"), value = "black"),
          sliderInput("gridSize", label = h4("size"), min = 0.1, max = 5, value = 1, step = 0.1),
          textInput("gridLinetype", label = h4("linetype"), value = "longdash")
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
      inputId = "plotType", label = h3("Plot type"),
      choices = list("initializePlot" = 1, "addScatter" = 2, "addLine" = 3),
      selected = 1
    )
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
    watermark <- Label$new(text = input$watermarkText,
                           size = input$watermarkSize,
                           color = input$watermarkColor)
    updatedTheme <- Theme$new(themesProperties = updatedThemeProperties,
                              watermark = watermark)
    # Update some Theme properties
    updatedTheme$titleFont$size <- input$titleSize
    updatedTheme$subtitleFont$size <- input$subtitleSize
    # TO DO: Find and fix issue
    #updatedTheme$xlabelFont$size <- input$xlabelSize
    #updatedTheme$ylabelFont$size <- input$ylabelSize
    
    plotConfiguration <- PlotConfiguration$new(
      title = "Title",
      subtitle = "Subtitle",
      xlabel = Label$new("X-label", color = input$xlabelColor, size = input$xlabelSize),
      ylabel = Label$new("Y-label", color = input$ylabelColor, size = input$ylabelSize),
      theme = updatedTheme
    )
    
    x <- seq(-2, 2, 0.1)
    y <- cos(x)
    if (input$plotType == 1) {
      samplePlot <- initializePlot(plotConfiguration = plotConfiguration)
    }
    if (input$plotType == 2) {
      samplePlot <- addScatter(x = x, y = y, plotConfiguration = plotConfiguration)
    }
    if (input$plotType == 3) {
      samplePlot <- addLine(x = x, y = y, plotConfiguration = plotConfiguration)
    }
    samplePlot
  })
}

shinyApp(ui = ui, server = server)
