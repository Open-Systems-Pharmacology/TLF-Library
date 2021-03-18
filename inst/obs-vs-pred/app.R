require(shiny)
require(tlf)

# Use theme template but this could be modified in later versions
useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("Obs vs Pred Plot", align = "center"),
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
          uiOutput("groupingVariableNames"),
          uiOutput("uncertaintyVariableNames"),
          uiOutput("lloqVariableNames")
        )
      ),
      tabPanel(
        "Analysis",
        fluidRow(
          selectInput("regression", label = "Regression", choices = list(none = "none", linear = "lm", loess = "loess"))
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
          selectInput("yAxisScale", label = "scale", choices = Scaling, selected = Scaling$lin)
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
    plotOutput(outputId = "obsVsPredPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    br(),
    tableOutput(outputId = "obsVsPredTable"),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getObsVsPredData <- reactive({
    obsVsPredData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        obsVsPredData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        obsVsPredData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(obsVsPredData)
  })
  
  getVariableNames <- reactive({
    obsVsPredData <- getObsVsPredData()
    return(sapply(names(obsVsPredData), identity))
  })
  
  output$yVariableNames <- renderUI({
    selectInput("yVariableNames2", "Variable in Y", getVariableNames())
  })
  
  output$xVariableNames <- renderUI({
    selectInput("xVariableNames2", "Variable in X", getVariableNames())
  })

  output$groupingVariableNames <- renderUI({
    selectInput("groupingVariableNames2", "Group variable", c("none" = "none", getVariableNames()))
  })
  
  output$uncertaintyVariableNames <- renderUI({
    selectInput("uncertaintyVariableNames2", "Variable for error bars", c("none" = "none", getVariableNames()))
  })
  output$lloqVariableNames <- renderUI({
    selectInput("lloqVariableNames2", "Variable for LLOQ", c("none" = "none", getVariableNames()))
  })
  
  getDataMapping <- reactive({
    colorMapping <- input$groupingVariableNames2
    uncertaintyMapping <- input$uncertaintyVariableNames2
    lloqMapping <- input$lloqVariableNames2
    if(isIncluded(colorMapping, "none")){colorMapping <- NULL}
    if(isIncluded(uncertaintyMapping,"none")){uncertaintyMapping <- NULL}
    if(isIncluded(lloqMapping,"none")){lloqMapping <- NULL}
    dataMapping <- ObsVsPredDataMapping$new(x = input$xVariableNames2,
                                            y = input$yVariableNames2,
                                            color = colorMapping,
                                            uncertainty = uncertaintyMapping,
                                            lloq = lloqMapping)
    smoother <- input$regression
    if(smoother=="none"){smoother <- NULL}
    dataMapping$smoother <- smoother
    return(dataMapping)
  })

  output$obsVsPredPlot <- renderPlot({
    # Get the data
    obsVsPredData <- getObsVsPredData()
    if(!isOfType(obsVsPredData, "data.frame")){return()}
    if(nrow(obsVsPredData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    obsVsPredPlot <- plotObsVsPred(
      data = obsVsPredData,
      dataMapping = dataMapping
    )
    xLimits <- c(input$xAxisMin, input$xAxisMax)
    yLimits <- c(input$yAxisMin, input$yAxisMax)
    if(any(is.na(xLimits))){xLimits <- NULL}
    if(any(is.na(yLimits))){yLimits <- NULL}
    
    obsVsPredPlot <- setXAxis(obsVsPredPlot,
      limits = xLimits,
      scale = input$xAxisScale
    )
    obsVsPredPlot <- setYAxis(obsVsPredPlot,
      limits = yLimits,
      scale = input$yAxisScale
    )
    if(input$xLabel != ""){obsVsPredPlot <- setPlotLabels(obsVsPredPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){obsVsPredPlot <- setPlotLabels(obsVsPredPlot, ylabel = input$yLabel)}
    obsVsPredPlot <- setWatermark(obsVsPredPlot, watermark = input$watermark)
    obsVsPredPlot
  })
  
  output$obsVsPredTable = renderTable({
    obsVsPredData <- getObsVsPredData() 
    if(!isOfType(obsVsPredData, "data.frame")){return(data.frame())}
    if(nrow(obsVsPredData)==0){return(data.frame())}
    dataMapping <- getDataMapping()
    R2 <- 1 - var(obsVsPredData[,dataMapping$y]-obsVsPredData[,dataMapping$x])/var(obsVsPredData[,dataMapping$x])
    MFE <- 10^(mean(log10(obsVsPredData[,dataMapping$y])-log10(obsVsPredData[,dataMapping$x])))
    obsVsPredTable <- data.frame(Evaluation = c("R2", "MFE"),
                                 Value = c(R2, MFE))
    return(obsVsPredTable)
  })
}

shinyApp(ui = ui, server = server)
