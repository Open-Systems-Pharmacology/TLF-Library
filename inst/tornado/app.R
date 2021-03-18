require(shiny)
require(tlf)

# Use theme template but this could be modified in later versions
useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("Tornado Plot", align = "center"),
  column(
    5,
    tabsetPanel(
      tabPanel(
        "Data",
        fluidRow(
          selectInput("dataType",
            label = h4("Data location"),
            choices = list("environment" = "environment", "file" = "file")
          ),
          conditionalPanel(
            condition = "input.dataType == 'environment'",
            selectInput("dataFromEnv", label = h4("List of environment variables"), choices = sapply(ls(envir = sys.frame()), identity))
          ),
          conditionalPanel(
            condition = "input.dataType == 'file'",
            fileInput("dataFromFile", label = h4("Choose a csv file"), accept = ".csv")
          ),
          uiOutput("xVariableNames"),
          uiOutput("yVariableNames"),
          uiOutput("colorVariableNames"),
          conditionalPanel(
            condition = "input.bar == 'No'",
            uiOutput("shapeVariableNames")
          )
        )
      ),
      tabPanel(
        "Analysis",
        fluidRow(
          selectInput("bar", label = "Tornado as bar plot", choices = list(Yes = "Yes", No = "No")),
          selectInput("sort", label = "Sort values", choices = list(Yes = "Yes", No = "No")),
          selectInput("colorPalette", label = "Color palette", choices = list(none = "none",
                                                                      Spectral = "Spectral",
                                                                      Greys = "Greys",
                                                                      Blues = "Blues",
                                                                      RedToBlue = "RdBu",
                                                                      RedToGreen = "RdYlGn"))
        )
        ),
      tabPanel(
        "Axes",
        fluidRow(
          h4("x axis"),
          numericInput("xAxisMin", label = "min", value = NULL),
          numericInput("xAxisMax", label = "max", value = NULL),
          selectInput("xAxisScale", label = "scale", choices = Scaling, selected = Scaling$lin)
        )
      ),
      tabPanel(
        "Labels",
        fluidRow(
          textInput("xLabel", label = "x label", value = NULL),
          textInput("yLabel", label = "y label", value = NULL),
          textInput("watermark", label = "watermark", value = NULL)
        )
      )
    )
  ),

  # Plot Column
  column(
    7,
    br(),
    plotOutput(outputId = "tornadoPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getTornadoData <- reactive({
    tornadoData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        tornadoData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        tornadoData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(tornadoData)
  })
  
  getVariableNames <- reactive({
    tornadoData <- getTornadoData()
    return(sapply(names(tornadoData), identity))
  })
  
  output$yVariableNames <- renderUI({
    selectInput("yVariableNames2", "Variable in Y", getVariableNames())
  })
  
  output$xVariableNames <- renderUI({
    selectInput("xVariableNames2", "Variable in X", getVariableNames())
  })

  output$shapeVariableNames <- renderUI({
    selectInput("shapeVariableNames2", "Shape group variable", c("none" = "none", getVariableNames()))
  })
  
  output$colorVariableNames <- renderUI({
    selectInput("colorVariableNames2", "Color group variable", c("none" = "none", getVariableNames()))
  })
  
  getDataMapping <- reactive({
    colorMapping <- input$colorVariableNames2
    shapeMapping <- input$shapeVariableNames2
    if(isIncluded(colorMapping, "none")){colorMapping <- NULL}
    if(isIncluded(shapeMapping, "none")){shapeMapping <- NULL}
    dataMapping <- TornadoDataMapping$new(x = input$xVariableNames2, 
                                          y = input$yVariableNames2,
                                          color = colorMapping,
                                          fill = colorMapping,
                                          shape = shapeMapping)
    dataMapping$sorted <- (input$sort == 'Yes')
    return(dataMapping)
  })

  output$tornadoPlot <- renderPlot({
    # Get the data
    tornadoData <- getTornadoData()
    if(!isOfType(tornadoData, "data.frame")){return()}
    if(nrow(tornadoData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    colorPalette <- input$colorPalette
    if(colorPalette %in% "none"){colorPalette <- NULL}
    tornadoPlot <- plotTornado(
      data = tornadoData,
      dataMapping = dataMapping,
      bar = (input$bar == "Yes"),
      colorPalette = colorPalette
    )
    xLimits <- c(input$xAxisMin, input$xAxisMax)
    if(any(is.na(xLimits))){xLimits <- NULL}
    
    tornadoPlot <- setXAxis(tornadoPlot,
      limits = xLimits,
      scale = input$xAxisScale
    )
    
    if(input$xLabel != ""){tornadoPlot <- setPlotLabels(tornadoPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){tornadoPlot <- setPlotLabels(tornadoPlot, ylabel = input$yLabel)}
    tornadoPlot <- setWatermark(tornadoPlot, watermark = input$watermark)
    tornadoPlot
  })
}

shinyApp(ui = ui, server = server)
