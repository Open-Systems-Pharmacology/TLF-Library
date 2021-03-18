require(shiny)
require(tlf)

# Use theme template but this could be modified in later versions
useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("DDI Ratio Plot", align = "center"),
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
          sliderInput("ratioLine", label = "Ratio Limit", min = 1, max = 10, step = 0.1, value = 2),
          sliderInput("guestLine", label = "Guest et al. delta", min = 1, max = 2, step = 0.01, value = 1),
          selectInput("comparisonType", label = "Comparison type", choices = DDIComparisonTypes, selected = DDIComparisonTypes$obsVsPred)
        )
        ),
      tabPanel(
        "Axes",
        fluidRow(
          h4("x axis"),
          numericInput("xAxisMin", label = "min", value = NULL),
          numericInput("xAxisMax", label = "max", value = NULL),
          selectInput("xAxisScale", label = "scale", choices = Scaling, selected = Scaling$log),
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
    plotOutput(outputId = "ddiRatioPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    br(),
    tableOutput(outputId = "ddiRatioTable"),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getddiRatioData <- reactive({
    ddiRatioData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        ddiRatioData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        ddiRatioData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(ddiRatioData)
  })
  
  getVariableNames <- reactive({
    ddiRatioData <- getddiRatioData()
    return(sapply(names(ddiRatioData), identity))
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
    dataMapping <- DDIRatioDataMapping$new(x = input$xVariableNames2,
                                           y = input$yVariableNames2,
                                           color = colorMapping,
                                           shape = shapeMapping,
                                           uncertainty = uncertaintyMapping)
    dataMapping$lines$ddiRatio2 <- c(input$ratioLine, 1/input$ratioLine)
    dataMapping$lines$guestRatio <- input$guestLine
    dataMapping$comparisonType <- input$comparisonType
    return(dataMapping)
  })

  output$ddiRatioPlot <- renderPlot({
    # Get the data
    ddiRatioData <- getddiRatioData()
    if(!isOfType(ddiRatioData, "data.frame")){return()}
    if(nrow(ddiRatioData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    ddiRatioPlot <- plotDDIRatio(
      data = ddiRatioData,
      dataMapping = dataMapping
    )
    xLimits <- c(input$xAxisMin, input$xAxisMax)
    yLimits <- c(input$yAxisMin, input$yAxisMax)
    if(any(is.na(xLimits))){xLimits <- NULL}
    if(any(is.na(yLimits))){yLimits <- NULL}
    
    ddiRatioPlot <- setXAxis(ddiRatioPlot,
      limits = xLimits,
      scale = input$xAxisScale
    )
    ddiRatioPlot <- setYAxis(ddiRatioPlot,
      limits = yLimits,
      scale = input$yAxisScale
    )
    if(input$xLabel != ""){ddiRatioPlot <- setPlotLabels(ddiRatioPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){ddiRatioPlot <- setPlotLabels(ddiRatioPlot, ylabel = input$yLabel)}
    ddiRatioPlot <- setWatermark(ddiRatioPlot, watermark = input$watermark)
    ddiRatioPlot
  })
  
  output$ddiRatioTable = renderTable({
    ddiRatioData <- getddiRatioData() 
    if(!isOfType(ddiRatioData, "data.frame")){return(data.frame())}
    if(nrow(ddiRatioData)==0){return(data.frame())}
    dataMapping <- getDataMapping()
    #ddiRatioTable <- getDDIRatioMeasure(data=ddiRatioData, 
    #                                  dataMapping = dataMapping, 
    #                                  ratioLimits = c(input$ratioLine1, input$ratioLine2))
    #ddiRatioTable <- cbind.data.frame(" "= row.names(ddiRatioTable),
    #                                 ddiRatioTable)
    return(data.frame())
  })
}

shinyApp(ui = ui, server = server)
