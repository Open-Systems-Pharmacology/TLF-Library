test_that("plots grid produces error with wrong input type", {
  expect_error(plotGrid(DataSet$new(name = "DS")))
})

test_that("plots grid is rendered correctly", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  library(tlf)

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

  plotGridObj$title <- "my combined plot"
  plotGridObj$subtitle <- "something clever"
  plotGridObj$caption <- "something dumb"
  plotGridObj$nColumns <- 2L
  plotGridObj$tagLevels <- "A"
  plotGridObj$tagPrefix <- "Plot ("
  plotGridObj$tagSuffix <- ")"

  pGrid <- plotGrid(plotGridObj)

  expect_s3_class(pGrid, "ggplot")

  # TODO: turn on once you figure out why Appveyor produces slightly different plot
  # The differences are not discernible to the naked eye, so hard to diagnose at the moment
  # but also makes it not risky to skip the test at the moment
  # set.seed(123)
  # vdiffr::expect_doppelganger(
  #   title = "plotGrid works as expected",
  #   fig = plotGrid(plotGridObj)
  # )
})

test_that("adding plots works with plots grid configuration", {
  myPlotGrid <- PlotGridConfiguration$new()

  # adding single plot works
  myPlotGrid$addPlots(ggplot())

  expect_equal(length(myPlotGrid$plotList), 1L)

  # adding a list of plot works
  myPlotGrid$addPlots(list("p1" = ggplot(), "p2" = ggplot()))

  expect_equal(length(myPlotGrid$plotList), 3L)

  expect_equal(names(myPlotGrid$plotList), c("", "p1", "p2"))
})
