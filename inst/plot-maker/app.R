#---------- Initialization of app ----------#
# Load required packages
require(shiny)
require(tlf)

# Helper: list of available plots: some of these plots may require a different app
listOfAvailablePlots <- c("initializePlot", "addScatter", "addLine", "addRibbon", "addErrorbar",
                          "plotPKRatio", "plotDDIRatio", "plotBoxWhisker", "plotHistogram", 
                          "plotTimeProfile", "plotTornado", "plotObsVsPred")

# Functions not exported from tlf, but quite helpful
`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {lhs} else {rhs}
}
isOfLength <- function(object, nbElements) {
  return(length(object) == nbElements)
}
isOfType <- function(object, type) {
  if (is.null(object)) {return(FALSE)}
  any(class(object) %in% type)
}
isIncluded <- function(values, parentValues) {
  if (is.null(values)) {return(FALSE)}
  if (length(values)==0) {return(FALSE)}
  return(as.logical(min(values %in% parentValues)))
}
asMappingVariable <- function(variableName){
  if(isIncluded(variableName, "none")){return()}
  return(variableName)
}
asScale <- function(scale){
  if(isIncluded(scale, "identity")){return(Scaling$lin)}
  if(isIncluded(scale, "log10")){return(Scaling$log)}
  return(scale)
}

#---------- User interface ----------#
ui <- fluidPage(
  h1("Plot Maker", align = "center"),
  column(
    5,
    tabsetPanel(
      tabPanel(
        "Data",
        fluidRow(
          selectInput("dataType",
            label = "Location",
            choices = list("environment" = "environment", "file" = "file")
            ),
          conditionalPanel(
            condition = "input.dataType == 'environment'",
            selectInput("dataFromEnv", label = "Variables available in environment", choices = sapply(ls(envir = sys.frame()), identity))
            ),
          conditionalPanel(
            condition = "input.dataType == 'file'",
            fileInput("dataFromFile", label = "Select a .csv file", accept = ".csv")
            )
          ),
          fluidRow(
            tableOutput("dataTable"),
            style = "overflow-x: scroll"
            )
      ),
      tabPanel(
        "Data Mapping",
        uiOutput("xVariableNames"),
        uiOutput("yVariableNames"),
        uiOutput("yminVariableNames"),
        uiOutput("ymaxVariableNames"),
        uiOutput("colorVariableNames"),
        uiOutput("fillVariableNames"),
        uiOutput("shapeVariableNames"),
        uiOutput("linetypeVariableNames")
        ),
      tabPanel(
        "Analysis",
        conditionalPanel(
          condition = "input.selectedPlot == 'plotBoxWhisker'",
          fluidRow(tableOutput("summaryTable"), style = "overflow-x: scroll")
        ),
        conditionalPanel(
          condition = "input.selectedPlot == 'plotPKRatio'",
          fluidRow(tableOutput("pkRatioTable"), align = 'center')
        )
        ),
      tabPanel(
        "Plot Configuration",
        tabsetPanel(
          tabPanel("Labels",
                   tabsetPanel(
                     tabPanel("Title",
                              uiOutput("titleText"),
                              uiOutput("titleColor"),
                              uiOutput("titleSize"),
                              uiOutput("titleAngle")
                              ),
                     tabPanel("Subtitle",
                              uiOutput("subtitleText"),
                              uiOutput("subtitleColor"),
                              uiOutput("subtitleSize"),
                              uiOutput("subtitleAngle")
                              ),
                     tabPanel("Xlabel",
                              uiOutput("xlabelText"),
                              uiOutput("xlabelColor"),
                              uiOutput("xlabelSize"),
                              uiOutput("xlabelAngle")),
                     tabPanel("Ylabel",
                              uiOutput("ylabelText"),
                              uiOutput("ylabelColor"),
                              uiOutput("ylabelSize"),
                              uiOutput("ylabelAngle")
                              )
                     )
                   ),
          tabPanel("Background",
                   tabsetPanel(
                     tabPanel("Plot Area",
                              uiOutput("plotFill"),
                              uiOutput("plotColor"),
                              uiOutput("plotSize"),
                              uiOutput("plotLinetype")
                              ),
                     tabPanel("Panel Area",
                              uiOutput("panelFill"),
                              uiOutput("panelColor"),
                              uiOutput("panelSize"),
                              uiOutput("panelLinetype")
                              ),
                     tabPanel("XGrid",
                              uiOutput("xGridColor"),
                              uiOutput("xGridSize"),
                              uiOutput("xGridLinetype")
                              ),
                     tabPanel("YGrid",
                              uiOutput("yGridColor"),
                              uiOutput("yGridSize"),
                              uiOutput("yGridLinetype")
                              )
                     )
                   ),
          tabPanel("Watermark", 
                   uiOutput("watermarkText"),
                   uiOutput("watermarkColor"),
                   uiOutput("watermarkSize"),
                   uiOutput("watermarkAngle")
                   ),
          tabPanel("Axes",
                   tabsetPanel(
                     tabPanel("XAxis",
                              uiOutput("xAxisScale"),
                              uiOutput("xAxisLimitsMin"),
                              uiOutput("xAxisLimitsMax"),
                              uiOutput("xTicksColor"),
                              uiOutput("xTicksSize"),
                              uiOutput("xTicksAngle")
                              ),
                     tabPanel("YAxis",
                              uiOutput("yAxisScale"),
                              uiOutput("yAxisLimitsMin"),
                              uiOutput("yAxisLimitsMax"),
                              uiOutput("yTicksColor"),
                              uiOutput("yTicksSize"),
                              uiOutput("yTicksAngle")
                              )
                     )
                   ),
          tabPanel("Legend", uiOutput("legendPosition")),
          tabPanel("Theme",fileInput("loadTheme", label = "Use theme from .json", accept = ".json"))
        )
      )
    )
  ),

  # Plot Column
  column(
    7,
    br(),
    selectInput("selectedPlot", label = "Select a plot", choices = listOfAvailablePlots),
    br(),
    plotOutput(outputId = "displayPlot"),
    br(),
    downloadButton("savedPlot", "Save Plot"),
    align = "center"
  )
)

#---------- Server ----------#
server <- function(input, output) {
#---------- Reactive helpers  ----------#  
  #---------- Data ----------#  
  getData <- reactive({
    data <- data.frame(`No data` = NULL)
    if (isIncluded(input$dataType, "environment")) {
      if (!isIncluded(input$dataFromEnv, "")) {
        data <- get(input$dataFromEnv, envir = sys.frame())
        if(!(isOfType(data, "data.frame"))){
          data <- data.frame(`Data is not a data.frame` = NULL)
        }
      }
    }
    if (isIncluded(input$dataType, "file")) {
      if (!isOfLength(input$dataFromFile,0)) {
        data <- read.csv(input$dataFromFile$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      }
    }
    return(data)
  })
  
  output$dataTable <- renderTable({
    utils::head(getData())
  })
  
  #---------- Data Mapping ----------#  
  getVariableNames <- reactive({
    data <- getData()
    return(c("none" = "none", sapply(names(data), identity)))
  })
  
  output$xVariableNames <- renderUI({
    selectInput("xVariableNames2", "x variable", getVariableNames(), selected = input$xVariableNames2)
    })
  output$yVariableNames <- renderUI({
    if(isIncluded(input$selectedPlot, c("plotHistogram", "addRibbon", "addErrorbar"))){return()}
    selectInput("yVariableNames2", "y variable", getVariableNames(), selected = input$yVariableNames2)
    })
  output$yminVariableNames <- renderUI({
    if(!isIncluded(input$selectedPlot, c("addRibbon", "addErrorbar", "plotTimeProfile"))){return()}
    selectInput("yminVariableNames2", "ymin variable", getVariableNames(), selected = input$yminVariableNames2)
  })
  output$ymaxVariableNames <- renderUI({
    if(!isIncluded(input$selectedPlot, c("addRibbon", "addErrorbar", "plotTimeProfile"))){return()}
    selectInput("ymaxVariableNames2", "ymax variable", getVariableNames(), selected = input$ymaxVariableNames2)
  })
  output$colorVariableNames <- renderUI({
    if(isIncluded(input$selectedPlot, c("addRibbon","plotHistogram","plotBoxWhisker","plotTornado"))){return()}
    selectInput("colorVariableNames2", "color variable", getVariableNames(), selected = input$colorVariableNames2)
  })
  output$fillVariableNames <- renderUI({
    if(isIncluded(input$selectedPlot, c("addScatter", "addLine", "addErrorbar","plotPKRatio","plotDDIRatio","plotObsVsPred"))){return()}
    selectInput("fillVariableNames2", "fill variable", getVariableNames(), selected = input$fillVariableNames2)
  })
  output$shapeVariableNames <- renderUI({
    if(isIncluded(input$selectedPlot, c("addRibbon", "addErrorbar","plotHistogram","plotBoxWhisker","plotTornado"))){return()}
    selectInput("shapeVariableNames2", "shape variable", getVariableNames(), selected = input$shapeVariableNames2)
  })
  
  getDataMapping <- reactive({
    mappingVariables <- c("x", "y", "ymin", "ymax", "color", "fill", "shape", "linetype")
    mappingExpression <- parse(text = paste0(
      mappingVariables, "Variable <- asMappingVariable(input$", mappingVariables, "VariableNames2)"
    ))
    eval(mappingExpression)
    
    dataMapping <- switch(input$selectedPlot,
                          addScatter = XYGDataMapping$new(x=xVariable,y=yVariable,color=colorVariable,shape=shapeVariable),
                          addLine = XYGDataMapping$new(x=xVariable,y=yVariable,color=colorVariable,linetype=linetypeVariable),
                          addRibbon = RangeDataMapping$new(x=xVariable,ymin=yVariable,ymax=yVariable,fill=fillVariable),
                          addErrorbar = RangeDataMapping$new(x=xVariable,ymin=yVariable,ymax=yVariable,color=colorVariable),
                          plotPKRatio = PKRatioDataMapping$new(x=xVariable,y=yVariable,color=colorVariable,shape=shapeVariable),
                          plotDDIRatio = DDIRatioDataMapping$new(x=xVariable,y=yVariable,color=colorVariable,shape=shapeVariable),
                          plotBoxWhisker = BoxWhiskerDataMapping$new(x=xVariable,y=yVariable,fill=fillVariable),
                          plotHistogram = HistogramDataMapping$new(x=xVariable,y=yVariable,fill=fillVariable),
                          plotTimeProfile = TimeProfileDataMapping$new(x=xVariable,y=yVariable),
                          plotTornado = TornadoDataMapping$new(x=xVariable,y=yVariable,fill=fillVariable),
                          plotObsVsPred = ObsVsPredDataMapping$new(x=xVariable,y=yVariable,color=colorVariable,shape=shapeVariable),
                          NULL)
    return(dataMapping)
  })
  
  #---------- Update Plot ----------#
  # Use a theme
  updateTheme <- reactive({
    if (length(input$loadTheme)==0) {return(invisible())}
    useTheme(loadThemeFromJson(input$loadTheme$datapath))
  })
  # Get watermark features
  output$watermarkText <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("watermarkText2", label = "text", value = displayPlot$plotConfiguration$background$watermark$text)
  })
  output$watermarkColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("watermarkColor2", label = "color", value = displayPlot$plotConfiguration$background$watermark$font$color)
  })
  output$watermarkSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("watermarkSize2", label = "size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$background$watermark$font$size)
  })
  output$watermarkAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("watermarkAngle2", label = "angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$background$watermark$font$angle)
  })
  
  # Get title features
  output$titleText <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("titleText2", label = "text", value = displayPlot$plotConfiguration$labels$title$text)
  })
  output$titleColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("titleColor2", label = "color", value = displayPlot$plotConfiguration$labels$title$font$color)
  })
  output$titleSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("titleSize2", label = "size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$labels$title$font$size)
  })
  output$titleAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("titleAngle2", label = "angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$labels$title$font$angle)
  })
  
  # Get subtitle features
  output$subtitleText <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("subtitleText2", label = "text", value = displayPlot$plotConfiguration$labels$subtitle$text)
  })
  output$subtitleColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("subtitleColor2", label = "color", value = displayPlot$plotConfiguration$labels$subtitle$font$color)
  })
  output$subtitleSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("subtitleSize2", label = "size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$labels$subtitle$font$size)
  })
  output$subtitleAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("subtitleAngle2", label = "angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$labels$subtitle$font$angle)
  })
  
  # Get xlabel features
  output$xlabelText <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("xlabelText2", label = "text", value = displayPlot$plotConfiguration$labels$xlabel$text)
  })
  output$xlabelColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("xlabelColor2", label = "color", value = displayPlot$plotConfiguration$labels$xlabel$font$color)
  })
  output$xlabelSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("xlabelSize2", label = "size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$labels$xlabel$font$size)
  })
  output$xlabelAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("xlabelAngle2", label = "angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$labels$xlabel$font$angle)
  })
  
  # Get ylabel features
  output$ylabelText <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("ylabelText2", label = "text", value = displayPlot$plotConfiguration$labels$ylabel$text)
  })
  output$ylabelColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("ylabelColor2", label = "color", value = displayPlot$plotConfiguration$labels$ylabel$font$color)
  })
  output$ylabelSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("ylabelSize2", label = "size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$labels$ylabel$font$size)
  })
  output$ylabelAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("ylabelAngle2", label = "angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$labels$ylabel$font$angle)
  })
  
  # Get plot features
  output$plotFill <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("plotFill2", label = "fill", value = displayPlot$plotConfiguration$background$plot$fill)
  })
  output$plotColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("plotColor2", label = "color", value = displayPlot$plotConfiguration$background$plot$color)
  })
  output$plotSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("plotSize2", label = "size", min = 0, max = 5, step = 0.05,
                value = displayPlot$plotConfiguration$background$plot$size)
  })
  output$plotLinetype <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("plotLinetype2", label = "linetype", choices = Linetypes,
                selected = displayPlot$plotConfiguration$background$plot$linetype)
  })
  
  # Get panel features
  output$panelFill <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("panelFill2", label = "fill", value = displayPlot$plotConfiguration$background$panel$fill)
  })
  output$panelColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("panelColor2", label = "color", value = displayPlot$plotConfiguration$background$panel$color)
  })
  output$panelSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("panelSize2", label = "size", min = 0, max = 5, step = 0.05,
                value = displayPlot$plotConfiguration$background$panel$size)
  })
  output$panelLinetype <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("panelLinetype2", label = "linetype", choices = Linetypes,
                selected = displayPlot$plotConfiguration$background$panel$linetype)
  })
  
  # Get xGrid features
  output$xGridColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("xGridColor2", label = "color", value = displayPlot$plotConfiguration$background$xGrid$color)
  })
  output$xGridSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("xGridSize2", label = "size", min = 0, max = 5, step = 0.05,
                value = displayPlot$plotConfiguration$background$xGrid$size)
  })
  output$xGridLinetype <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("xGridLinetype2", label = "linetype", choices = Linetypes,
                selected = displayPlot$plotConfiguration$background$xGrid$linetype)
  })
  
  # Get yGrid features
  output$yGridColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("yGridColor2", label = "color", value = displayPlot$plotConfiguration$background$yGrid$color)
  })
  output$yGridSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("yGridSize2", label = "size", min = 0, max = 5, step = 0.05,
                value = displayPlot$plotConfiguration$background$yGrid$size)
  })
  output$yGridLinetype <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("yGridLinetype2", label = "linetype", choices = Linetypes,
                selected = displayPlot$plotConfiguration$background$yGrid$linetype)
  })
  
  # Get xAxis features
  output$xAxisScale <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("xAxisScale2", label = "scale", choices = Scaling,
                selected = asScale(displayPlot$plotConfiguration$xAxis$scale))
  })
  output$xAxisLimitsMin <- renderUI({
    displayPlot <- getDisplayPlot()
    limits <- displayPlot$plotConfiguration$xAxis$limit %||% NA
    numericInput("xAxisLimitsMin2", label = "xmin", value = min(limits))
  })
  output$xAxisLimitsMax <- renderUI({
    displayPlot <- getDisplayPlot()
    limits <- displayPlot$plotConfiguration$xAxis$limit %||% NA
    numericInput("xAxisLimitsMax2", label = "xmax", value = max(limits))
  })
  output$xTicksColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("xTicksColor2", label = "ticklabels color", value = displayPlot$plotConfiguration$xAxis$font$color)
  })
  output$xTicksSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("xTicksSize2", label = "ticklabels size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$xAxis$font$size)
  })
  output$xTicksAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("xTicksAngle2", label = "ticklabels angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$xAxis$font$angle)
  })
  
  # Get yAxis features
  output$yAxisScale <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("yAxisScale2", label = "scale", choices = Scaling,
                selected = asScale(displayPlot$plotConfiguration$yAxis$scale))
  })
  output$yAxisLimitsMin <- renderUI({
    displayPlot <- getDisplayPlot()
    limits <- displayPlot$plotConfiguration$yAxis$limit %||% NA
    numericInput("yAxisLimitsMin2", label = "ymin", value = min(limits))
  })
  output$yAxisLimitsMax <- renderUI({
    displayPlot <- getDisplayPlot()
    limits <- displayPlot$plotConfiguration$yAxis$limit %||% NA
    numericInput("yAxisLimitsMax2", label = "ymax", value = max(limits))
  })
  output$yTicksColor <- renderUI({
    displayPlot <- getDisplayPlot()
    textInput("yTicksColor2", label = "ticklabels color", value = displayPlot$plotConfiguration$yAxis$font$color)
  })
  output$yTicksSize <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("yTicksSize2", label = "ticklabels size", min = 0, max = 48, step = 0.5,
                value = displayPlot$plotConfiguration$yAxis$font$size)
  })
  output$yTicksAngle <- renderUI({
    displayPlot <- getDisplayPlot()
    sliderInput("yTicksAngle2", label = "ticklabels angle", min = -180, max = 180, step = 1,
                value = displayPlot$plotConfiguration$yAxis$font$angle)
  })
  
  # Get legend features
  output$legendPosition <- renderUI({
    displayPlot <- getDisplayPlot()
    selectInput("legendPosition2", label = "position", choices = LegendPositions,
                selected = displayPlot$plotConfiguration$legend$position)
  })
  
  getDisplayPlot <- reactive({
    displayPlot <- initializePlot()
    data <- getData()
    if(nrow(data)==0){return(displayPlot)}
    dataMapping <- getDataMapping()
    displayPlot <- switch(input$selectedPlot,
                          addScatter = addScatter(data = data, dataMapping = dataMapping),
                          addLine = addLine(data = data, dataMapping = dataMapping),
                          addRibbon = addRibbon(data = data, dataMapping = dataMapping),
                          addErrorbar = addErrorbar(data = data, dataMapping = dataMapping),
                          plotPKRatio = plotPKRatio(data = data, dataMapping = dataMapping),
                          plotDDIRatio = plotDDIRatio(data = data, dataMapping = dataMapping),
                          plotBoxWhisker = plotBoxWhisker(data = data, dataMapping = dataMapping),
                          plotHistogram = plotHistogram(data = data, dataMapping = dataMapping),
                          plotTimeProfile = plotTimeProfile(data = data, dataMapping = dataMapping),
                          plotTornado = plotTornado(data = data, dataMapping = dataMapping),
                          plotObsVsPred = plotObsVsPred(data = data, dataMapping = dataMapping),
                          initializePlot())
    return(displayPlot)
  })
  
  #---------- Display Plot ----------#
  getUpdatedPlot <- reactive({
    updateTheme()
    displayPlot <- getDisplayPlot()
    
    #---------- Update plot based on Plot Configuration ----------#
    displayPlot <- setWatermark(displayPlot,
                                watermark = input$watermarkText2,
                                color = input$watermarkColor2,
                                size = input$watermarkSize2,
                                angle = input$watermarkAngle2)
    
    displayPlot <- setPlotLabels(displayPlot,
                                 title = Label$new(text = input$titleText2, 
                                                   font = Font$new(color = input$titleColor2, size = input$titleSize2, angle = input$titleAngle2)),
                                 subtitle = Label$new(text = input$subtitleText2, 
                                                  font = Font$new(color = input$subtitleColor2, size = input$subtitleSize2, angle = input$subtitleAngle2)),
                                 xlabel = Label$new(text = input$xlabelText2, 
                                                    font = Font$new(color = input$xlabelColor2, size = input$xlabelSize2, angle = input$xlabelAngle2)),
                                 ylabel = Label$new(text = input$ylabelText2, 
                                                    font = Font$new(color = input$ylabelColor2, size = input$ylabelSize2, angle = input$ylabelAngle2)))
    
    displayPlot <- setBackgroundPlotArea(displayPlot,
                                         fill = input$plotFill2, 
                                         color = input$plotColor2,
                                         size = input$plotSize2, 
                                         linetype = input$plotLinetype2)
    
    displayPlot <- setBackgroundPanelArea(displayPlot,
                                          fill = input$panelFill2, 
                                          color = input$panelColor2,
                                          size = input$panelSize2, 
                                          linetype = input$panelLinetype2)
    
    displayPlot <- setLegendPosition(displayPlot, position = input$legendPosition2)
    
    displayPlot <- setXGrid(displayPlot,
                            color = input$xGridColor2,
                            size = input$xGridSize2, 
                            linetype = input$xGridLinetype2)
    
    displayPlot <- setYGrid(displayPlot,
                            color = input$yGridColor2,
                            size = input$yGridSize2, 
                            linetype = input$yGridLinetype2)
    
    displayPlot <- setXAxis(displayPlot,
                            scale = input$xAxisScale2,
                            font = Font$new(color=input$xTicksColor2,size=input$xTicksSize2,angle=input$xTicksAngle2))
    
    displayPlot <- setYAxis(displayPlot,
                            scale = input$yAxisScale2,
                            font = Font$new(color=input$yTicksColor2,size=input$yTicksSize2,angle=input$yTicksAngle2))
  })
  
  # Plot displayed after update
  output$displayPlot <- renderPlot({
    displayPlot <- getUpdatedPlot()
    return(displayPlot)
    })
  
  #---------- Analysis ----------#
  output$pkRatioTable <- renderTable({
    data <- getData()
    dataMapping <- getDataMapping()
    getPKRatioMeasure(data = data, dataMapping = dataMapping)
  },
  rownames = TRUE)
  
  output$summaryTable <- renderTable({
    data <- getData()
    dataMapping <- getDataMapping()
    getBoxWhiskerMeasure(data = data, dataMapping = dataMapping)
  },
  rownames = TRUE)
  
  #---------- Save Plot ----------#
  output$savedPlot <- downloadHandler(
    filename = "savedPlot.png",
    content = function(file) {
      displayPlot <- getUpdatedPlot()
      ggsave(filename = file, plot = displayPlot)
    }
  )
}

shinyApp(ui = ui, server = server)
