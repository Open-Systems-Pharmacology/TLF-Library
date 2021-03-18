require(shiny)
require(tlf)

useTheme(loadThemeFromJson(system.file("extdata", "template-theme.json", package = "tlf")))

# Define UI page ----
ui <- fluidPage(
  h1("Box Whisker Plot", align = "center"),
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
          uiOutput("yVariableNames"),
          uiOutput("xVariableNames"),
          uiOutput("fillVariableNames")
        )
      ),
      tabPanel(
        "Analysis",
        fluidRow(
          selectInput("ymax", label = "Max whisker", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile95%`),
          selectInput("upper", label = "Box upper edge", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile75%`),
          selectInput("middle", label = "Middle edge", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile50%`),
          selectInput("lower", label = "Box lower edge", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile25%`),
          selectInput("ymin", label = "Min whisker", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile5%`),
          br(),
          selectInput("outliers", label = "Flag outliers", choices = list(Yes = TRUE, No = FALSE)),
          conditionalPanel(
            condition = "input.outliers == 'TRUE'",
            selectInput("maxOutliers", label = "Max value outlier flag", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile75%+1.5IQR`),
            selectInput("minOutliers", label = "Min value outlier flag", choices = tlfStatFunctions, selected = tlfStatFunctions$`Percentile25%-1.5IQR`)
          )
        )
        ),
      tabPanel(
        "Axes",
        fluidRow(
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
    plotOutput(outputId = "boxWhiskerPlot"),
    br(),
    actionButton("exportButton", "Save Plot"),
    br(),
    align = "center"
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  getBoxWhiskerData <- reactive({
    boxplotData <- data.frame(`Fill data first` = NULL)
    if (input$dataType == "environment") {
      if (input$dataFromEnv != "") {
        boxplotData <- get(input$dataFromEnv, envir = sys.frame())
      }
    }
    if (input$dataType == "file") {
      if (!isOfLength(input$dataFromFile, 0)) {
        boxplotData <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE)
      }
    }
    return(boxplotData)
  })
  
  getVariableNames <- reactive({
    boxplotData <- getBoxWhiskerData()
    return(sapply(names(boxplotData), identity))
  })
  
  output$yVariableNames <- renderUI({
    selectInput("yVariableNames2", "Variable in Y", getVariableNames())
  })
  
  output$xVariableNames <- renderUI({
    selectInput("xVariableNames2", "Group variable in X", choices = c("none" = "none", getVariableNames()))
  })

  output$fillVariableNames <- renderUI({
    selectInput("fillVariableNames2", "Color group variable", choices = c("none" = "none", getVariableNames()))
  })
  
  getDataMapping <- reactive({
    xMapping <- input$xVariableNames2
    fillMapping <- input$fillVariableNames2
    if(isIncluded(xMapping, "none")){xMapping <- NULL}
    if(isIncluded(fillMapping, "none")){fillMapping <- NULL}
    
    dataMapping <- BoxWhiskerDataMapping$new(x = xMapping,
                                             y = input$yVariableNames2,
                                             color = fillMapping,
                                             fill = fillMapping,
                                             ymax = input$ymax,
                                             upper = input$upper,
                                             middle = input$middle,
                                             lower = input$lower,
                                             maxOutlierLimit = input$maxOutliers,
                                             minOutlierLimit = input$minOutliers)
    return(dataMapping)
  })

  output$boxWhiskerPlot <- renderPlot({
    # Get the data
    boxplotData <- getBoxWhiskerData()
    if(!isOfType(boxplotData, "data.frame")){return()}
    if(nrow(boxplotData)==0){return()}
    # Meta Data
    # DataMapping
    dataMapping <- getDataMapping()
    # PlotConfiguration
    boxWhiskerPlot <- plotBoxWhisker(
      data = boxplotData,
      dataMapping = dataMapping,
      outliers = as.logical(input$outliers)
    )
    yLimits <- c(input$yAxisMin, input$yAxisMax)
    if(any(is.na(yLimits))){yLimits <- NULL}
    
    boxWhiskerPlot <- setYAxis(boxWhiskerPlot,
      limits = yLimits,
      scale = input$yAxisScale
    )
    if(input$xLabel != ""){boxWhiskerPlot <- setPlotLabels(boxWhiskerPlot, xlabel = input$xLabel)}
    if(input$yLabel != ""){boxWhiskerPlot <- setPlotLabels(boxWhiskerPlot, ylabel = input$yLabel)}
    boxWhiskerPlot <- setWatermark(boxWhiskerPlot, watermark = input$watermark)
    boxWhiskerPlot
  })
}

shinyApp(ui = ui, server = server)
