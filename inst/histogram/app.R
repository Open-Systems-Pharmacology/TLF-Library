require(shiny)
require(tlf)

useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("Histogram", align = "center"),
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
          uiOutput("fillVariableNames")
        )
      ),
      tabPanel(
        "Analysis",
        fluidRow(
          uiOutput("bins"),
          selectInput("stack", label = "Stack bars", choices = list(Yes = "Yes", No = "No")),
          selectInput("fitNormalDist", label = "Fit normal distribution", choices = list(Yes = "Yes", No = "No"))
          )
        ),
      tabPanel(
        "Axes",
        fluidRow(
          h4("x axis"),
          numericInput("xAxisMin", label = "min", value = NULL),
          numericInput("xAxisMax", label = "max", value = NULL),
          selectInput("XAxisScale", label = "scale", choices = Scaling, selected = Scaling$lin),
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
    plotOutput(outputId = "histogramPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    br(),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getHistogramData <- reactive({
    histogramData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        histogramData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        histogramData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(histogramData)
  })
  
  getVariableNames <- reactive({
    histogramData <- getHistogramData()
    return(sapply(names(histogramData), identity))
  })
  
  output$xVariableNames <- renderUI({
    selectInput("xVariableNames2", "Group variable in X", choices = getVariableNames())
  })
  
  output$fillVariableNames <- renderUI({
    selectInput("fillVariableNames2", "Color group variable", choices = c("none" = "none", getVariableNames()))
  })
  
  output$bins <- renderUI({
    histogramData <- getHistogramData()
    sliderInput("bins2", "Number of bins", min = 1, max = max(10, nrow(histogramData)), step = 1, value = 10)
  })
  
  getDataMapping <- reactive({
    xMapping <- input$xVariableNames2
    fillMapping <- input$fillVariableNames2
    if(isIncluded(xMapping, "none")){xMapping <- NULL}
    if(isIncluded(fillMapping, "none")){fillMapping <- NULL}
    
    dataMapping <- HistogramDataMapping$new(stack = as.logical(input$stack %in% "Yes"),
                                            x = input$xVariableNames2,
                                            fill = fillMapping,
                                            bins = input$bins2,
                                            fitNormalDist = as.logical(input$fitNormalDist %in% "Yes"))
    return(dataMapping)
  })

  output$histogramPlot <- renderPlot({
    # Get the data
    histogramData <- getHistogramData()
    if(!isOfType(histogramData, "data.frame")){return()}
    if(nrow(histogramData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    histogramPlot <- plotHistogram(
      data = histogramData,
      dataMapping = dataMapping
    )
    xLimits <- c(input$xAxisMin, input$xAxisMax)
    yLimits <- c(input$yAxisMin, input$yAxisMax)
    if(any(is.na(xLimits))){xLimits <- NULL}
    if(any(is.na(yLimits))){yLimits <- NULL}
    
    histogramPlot <- setXAxis(histogramPlot,
                              limits = xLimits,
                              scale = input$xAxisScale
    )
    histogramPlot <- setYAxis(histogramPlot,
      limits = yLimits,
      scale = input$yAxisScale
    )
    if(input$xLabel != ""){histogramPlot <- setPlotLabels(histogramPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){histogramPlot <- setPlotLabels(histogramPlot, ylabel = input$yLabel)}
    histogramPlot <- setWatermark(histogramPlot, watermark = input$watermark)
    histogramPlot
  })
}

shinyApp(ui = ui, server = server)
