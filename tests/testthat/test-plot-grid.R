test_that("plots grid produces error with wrong input type", {
  expect_error(plotGrid(DataSet$new(name = "DS")))
})

set.seed(123)
ls_plots <- list(
  # first plot
  plotBoxWhisker(mtcars,
    dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"), outliers = FALSE
  ),
  # second plot
  plotBoxWhisker(ToothGrowth,
    dataMapping = BoxWhiskerDataMapping$new(x = "supp", y = "len")
  )
)

plotGridObj <- PlotGridConfiguration$new(ls_plots)

# specify further customizations for the plot grid
plotGridObj$title <- "my combined plot"
plotGridObj$subtitle <- "something clever"
plotGridObj$caption <- "my sources"
plotGridObj$nColumns <- 2L
plotGridObj$tagLevels <- "A"
plotGridObj$tagPrefix <- "Plot ("
plotGridObj$tagSuffix <- ")"
plotGridObj$tagColor <- "blue"
plotGridObj$tagSize <- 15
plotGridObj$tagAngle <- 45
plotGridObj$tagPosition <- TagPositions$top
plotGridObj$titleHorizontalJustification <- HorizontalJustification$middle
plotGridObj$subtitleHorizontalJustification <- HorizontalJustification$middle

test_that("plots grid is printed", {
  expect_visible(plotGridObj)
})

test_that("plots grid is rendered correctly", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  pGrid <- plotGrid(plotGridObj)

  expect_s3_class(pGrid, "ggplot")

  set.seed(123)
  vdiffr::expect_doppelganger(
    title = "plotGrid works as expected",
    fig = plotGrid(plotGridObj)
  )
})

test_that("adding plots works with plots grid configuration", {
  myPlotGrid <- PlotGridConfiguration$new()

  # adding single plot works
  myPlotGrid$addPlots(ggplot())

  expect_equal(length(myPlotGrid$plotList), 1L)

  # adding a list of plot works
  myPlotGrid$addPlots(list("p1" = ggplot(), "p2" = ggplot()))

  expect_equal(length(myPlotGrid$plotList), 3L)

  # adding a list of a single plot also works
  myPlotGrid$addPlots(list("p3" = ggplot()))

  expect_equal(length(myPlotGrid$plotList), 4L)

  expect_equal(names(myPlotGrid$plotList), c("", "p1", "p2", "p3"))
})

test_that("Plot Grid can have very long texts and contain plots with very long titles", {
  set.seed(123)
  ls_plots <- list(
    # first plot
    plotBoxWhisker(mtcars,
                   dataMapping = BoxWhiskerDataMapping$new(x = "am", y = "wt"),
                   outliers = FALSE,
                   plotConfiguration = BoxWhiskerPlotConfiguration$new(title = paste("Title: This is a", paste(rep("very", 40), collapse = " "), "long title"))
    ),
    # second plot
    plotBoxWhisker(ToothGrowth,
                   dataMapping = BoxWhiskerDataMapping$new(x = "supp", y = "len"),
                   plotConfiguration = BoxWhiskerPlotConfiguration$new(title = paste("Title: This is a", paste(rep("very", 40), collapse = " "), "long title"))
    )
  )

  plotGridObj <- PlotGridConfiguration$new(ls_plots)

  plotGridObj$tagLevels <- "A"
  plotGridObj$tagPrefix <- "Plot ("
  plotGridObj$tagSuffix <- ")"
  plotGridObj$tagColor <- "blue"

  plotGridObj$title <- paste("Title: This is a", paste(rep("very", 40), collapse = " "), "long title")
  plotGridObj$subtitle <- paste("Subtitle: This is a", paste(rep("very", 40), collapse = " "), "long subtitle")
  plotGridObj$caption <- paste("Caption: This is a", paste(rep("very", 40), collapse = " "), "long caption")
  plotGridObj$titleHorizontalJustification <- HorizontalJustification$middle
  plotGridObj$subtitleHorizontalJustification <- HorizontalJustification$left
  plotGridObj$captionHorizontalJustification <- HorizontalJustification$right

  vdiffr::expect_doppelganger("long labels in plotgrid",
                              fig = plotGrid(plotGridObj))
})
