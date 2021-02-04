context("Atom Plots")

test_that("Regular atom plots provide ggplot objects that includes a PlotConfiguration object", {
  emptyPlot <- initializePlot()
  scatterPlot <- addScatter(x=c(1,2,3),y=c(1,2,3))
  linePlot <- addLine(x=c(1,2,3),y=c(1,2,3))
  ribbonPlot <- addRibbon(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5))
  errorbarPlot <- addErrorbar(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5))
  
  expect_is(emptyPlot, "ggplot")
  expect_is(scatterPlot, "ggplot")
  expect_is(linePlot, "ggplot")
  expect_is(ribbonPlot, "ggplot")
  expect_is(errorbarPlot, "ggplot")
  
  expect_is(emptyPlot$plotConfiguration, "PlotConfiguration")
  expect_is(scatterPlot$plotConfiguration, "PlotConfiguration")
  expect_is(linePlot$plotConfiguration, "PlotConfiguration")
  expect_is(ribbonPlot$plotConfiguration, "PlotConfiguration")
  expect_is(errorbarPlot$plotConfiguration, "PlotConfiguration")
})

test_that("A PlotConfiguration input is correctlty used to create the plot when specified", {
  testPlotConfiguration <- PlotConfiguration$new(xlabel = "test X",
                                                 ylabel = "test Y",
                                                 watermark = "test watermark")
  emptyPlot <- initializePlot(plotConfiguration = testPlotConfiguration)
  scatterPlot <- addScatter(x=c(1,2,3),y=c(1,2,3),plotConfiguration = testPlotConfiguration)
  linePlot <- addLine(x=c(1,2,3),y=c(1,2,3),plotConfiguration = testPlotConfiguration)
  ribbonPlot <- addRibbon(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5),plotConfiguration = testPlotConfiguration)
  errorbarPlot <- addErrorbar(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5),plotConfiguration = testPlotConfiguration)
  
  expect_error(initializePlot(plotConfiguration = "testPlotConfiguration"))
  expect_error(addScatter(x=c(1,2,3),y=c(1,2,3),plotConfiguration = "testPlotConfiguration"))
  expect_error(addLine(x=c(1,2,3),y=c(1,2,3),plotConfiguration = "testPlotConfiguration"))
  expect_error(addRibbon(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5),plotConfiguration = "testPlotConfiguration"))
  expect_error(addErrorbar(x=c(1,2,3),ymin=c(1,2,3),ymax=c(3,4,5),plotConfiguration = "testPlotConfiguration"))
  
  # Same Labels
  expect_equivalent(emptyPlot$plotConfiguration$labels, testPlotConfiguration$labels)
  expect_equivalent(scatterPlot$plotConfiguration$labels, testPlotConfiguration$labels)
  expect_equivalent(linePlot$plotConfiguration$labels, testPlotConfiguration$labels)
  expect_equivalent(ribbonPlot$plotConfiguration$labels, testPlotConfiguration$labels)
  expect_equivalent(errorbarPlot$plotConfiguration$labels, testPlotConfiguration$labels)
  
  # Same Background
  expect_equivalent(emptyPlot$plotConfiguration$background, testPlotConfiguration$background)
  expect_equivalent(scatterPlot$plotConfiguration$background, testPlotConfiguration$background)
  expect_equivalent(linePlot$plotConfiguration$background, testPlotConfiguration$background)
  expect_equivalent(ribbonPlot$plotConfiguration$background, testPlotConfiguration$background)
  expect_equivalent(errorbarPlot$plotConfiguration$background, testPlotConfiguration$background)
  
  # Same xAxis
  expect_equivalent(emptyPlot$plotConfiguration$xAxis, testPlotConfiguration$xAxis)
  expect_equivalent(scatterPlot$plotConfiguration$xAxis, testPlotConfiguration$xAxis)
  expect_equivalent(linePlot$plotConfiguration$xAxis, testPlotConfiguration$xAxis)
  expect_equivalent(ribbonPlot$plotConfiguration$xAxis, testPlotConfiguration$xAxis)
  expect_equivalent(errorbarPlot$plotConfiguration$xAxis, testPlotConfiguration$xAxis)
  
  # Same yAxis
  expect_equivalent(emptyPlot$plotConfiguration$yAxis, testPlotConfiguration$yAxis)
  expect_equivalent(scatterPlot$plotConfiguration$yAxis, testPlotConfiguration$yAxis)
  expect_equivalent(linePlot$plotConfiguration$yAxis, testPlotConfiguration$yAxis)
  expect_equivalent(ribbonPlot$plotConfiguration$yAxis, testPlotConfiguration$yAxis)
  expect_equivalent(errorbarPlot$plotConfiguration$yAxis, testPlotConfiguration$yAxis)
})
