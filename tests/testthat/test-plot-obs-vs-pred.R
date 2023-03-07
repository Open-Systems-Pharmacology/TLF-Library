obsVsPredData <- data.frame(x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

plotConfiguration <- ObsVsPredPlotConfiguration$new(
  xScale = Scaling$log, xLimits = c(0.05, 50),
  yScale = Scaling$log, yLimits = c(0.05, 50),
)

test_that("plotObservedVsSimulated works", {
  vdiffr::expect_doppelganger(
    title = "basic",
    fig = plotObsVsPred(
      data = obsVsPredData,
      dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
      plotConfiguration = plotConfiguration
    )
  )
})


test_that("foldDistance are plotted correctly", {
  vdiffr::expect_doppelganger(
    title = "fold distance simple abline",
    fig = plotObsVsPred(
      data = obsVsPredData,
      dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
      plotConfiguration = plotConfiguration,
      foldDistance = 1
    )
  )

  vdiffr::expect_doppelganger(
    title = "fold distance one fold",
    fig = plotObsVsPred(
      data = obsVsPredData,
      dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
      plotConfiguration = plotConfiguration,
      foldDistance = 2
    )
  )

  vdiffr::expect_doppelganger(
    title = "fold distance several folds",
    fig = plotObsVsPred(
      data = obsVsPredData,
      dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
      plotConfiguration = plotConfiguration,
      foldDistance = c(1, 2, 3)
    )
  )

  vdiffr::expect_doppelganger(
    title = "fold distance many fols",
    fig = plotObsVsPred(
      data = obsVsPredData,
      dataMapping = ObsVsPredDataMapping$new(x = "x", y = "y"),
      plotConfiguration = plotConfiguration,
      foldDistance = c(1, 5, 10, 15, 20, 25)
    )
  )
})
