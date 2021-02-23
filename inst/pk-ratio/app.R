require(shiny)
require(tlf)

# Use theme template but this could be modified in later versions
useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("PK Ratio Plot", align = "center"),
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
          uiOutput("shapeVariableNames"),
          uiOutput("colorVariableNames"),
          uiOutput("uncertaintyVariableNames")
        )
      ),
      tabPanel(
        "Analysis",
        fluidRow(
          sliderInput("ratioLine1", label = "Ratio Limit #1", min = 1, max = 10, step = 0.1, value = 1.5),
          sliderInput("ratioLine2", label = "Ratio Limit #2", min = 1, max = 10, step = 0.1, value = 2)
        )
        ),
      tabPanel(
        "Axes",
        fluidRow(
          h4("x axis"),
          numericInput("xAxisMin", label = "min", value = NULL),
          numericInput("xAxisMax", label = "max", value = NULL),
          selectInput("xAxisScale", label = "scale", choices = Scaling, selected = Scaling$lin),
          h4("y axis"),
          numericInput("yAxisMin", label = "min", value = NULL),
          numericInput("yAxisMax", label = "max", value = NULL),
          selectInput("yAxisScale", label = "scale", choices = Scaling, selected = Scaling$log)
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
    plotOutput(outputId = "pkRatioPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    br(),
    tableOutput(outputId = "pkRatioTable"),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getPKRatioData <- reactive({
    pkRatioData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        pkRatioData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        pkRatioData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(pkRatioData)
  })
  
  getVariableNames <- reactive({
    pkRatioData <- getPKRatioData()
    return(sapply(names(pkRatioData), identity))
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
  
  output$uncertaintyVariableNames <- renderUI({
    selectInput("uncertaintyVariableNames2", "Variable for error bars", c("none" = "none", getVariableNames()))
  })
  
  getDataMapping <- reactive({
    colorMapping <- input$colorVariableNames2
    shapeMapping <- input$shapeVariableNames2
    uncertaintyMapping <- input$uncertaintyVariableNames2
    if(isIncluded(colorMapping, "none")){colorMapping <- NULL}
    if(isIncluded(shapeMapping, "none")){shapeMapping <- NULL}
    if(isIncluded(uncertaintyMapping,"none")){uncertaintyMapping <- NULL}
    dataMapping <- PKRatioDataMapping$new(x = input$xVariableNames2, 
                                          y = input$yVariableNames2,
                                          color = colorMapping,
                                          shape = shapeMapping,
                                          uncertainty = uncertaintyMapping)
    dataMapping$lines[[2]] <- c(input$ratioLine1, 1/input$ratioLine1)
    dataMapping$lines[[3]] <- c(input$ratioLine2, 1/input$ratioLine2)
    return(dataMapping)
  })

  output$pkRatioPlot <- renderPlot({
    # Get the data
    pkRatioData <- getPKRatioData()
    if(!isOfType(pkRatioData, "data.frame")){return()}
    if(nrow(pkRatioData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    pkRatioPlot <- plotPKRatio(
      data = pkRatioData,
      dataMapping = dataMapping
    )
    xLimits <- c(input$xAxisMin, input$xAxisMax)
    yLimits <- c(input$yAxisMin, input$yAxisMax)
    if(any(is.na(xLimits))){xLimits <- NULL}
    if(any(is.na(yLimits))){yLimits <- NULL}
    
    pkRatioPlot <- setXAxis(pkRatioPlot,
      limits = xLimits,
      scale = input$xAxisScale
    )
    pkRatioPlot <- setYAxis(pkRatioPlot,
      limits = yLimits,
      scale = input$yAxisScale
    )
    if(input$xLabel != ""){pkRatioPlot <- setPlotLabels(pkRatioPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){pkRatioPlot <- setPlotLabels(pkRatioPlot, ylabel = input$yLabel)}
    pkRatioPlot <- setWatermark(pkRatioPlot, watermark = input$watermark)
    pkRatioPlot
  })
  
  output$pkRatioTable = renderTable({
    pkRatioData <- getPKRatioData() 
    if(!isOfType(pkRatioData, "data.frame")){return(data.frame())}
    if(nrow(pkRatioData)==0){return(data.frame())}
    dataMapping <- getDataMapping()
    pkRatioTable <- getPKRatioMeasure(data=pkRatioData, 
                                      dataMapping = dataMapping, 
                                      ratioLimits = c(input$ratioLine1, input$ratioLine2))
    pkRatioTable <- cbind.data.frame(" "= row.names(pkRatioTable),
                                     pkRatioTable)
    return(pkRatioTable)
  })
}

shinyApp(ui = ui, server = server)
