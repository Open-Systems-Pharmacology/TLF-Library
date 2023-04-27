obsData <- data.frame(name = c("dataset1", "dataset1", "dataset2", "dataset2", "dataset2"),
                      x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 1, 3),
                      group = c("group A", "group A","group A", "group B", "group B"),
                      lloq = c(rep(0.5, 3), rep(1.5,2))
)

simTime <- seq(1, 10, 0.1)

simData <- data.frame(
  x = simTime,
  y = 10 * exp(-simTime),
  ymin = 8 * exp(-simTime),
  ymax = 12 * exp(-simTime)
)




test_that("plotTimeProfile works ", {

  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  color <- fill <- "group"
  linetype <- shape <- "name"

  vdiffr::expect_doppelganger(
    title = "basic",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      dataMapping = TimeProfileDataMapping$new(x = "x", y = "y",  ymin = "ymin", ymax = "ymax"),
      observedDataMapping = ObservedDataMapping$new(x = "x", y = "y", group = "group", shape = shape, color = shape)
    )
  )


  vdiffr::expect_doppelganger(
    title = "with lloq",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      dataMapping = TimeProfileDataMapping$new(x = "x", y = "y",  ymin = "ymin", ymax = "ymax"),
      observedDataMapping = ObservedDataMapping$new(x = "x", y = "y", group = "group", shape = shape, color = shape,lloq = "lloq")
    )
  )


  vdiffr::expect_doppelganger(
    title = "with lloq and plotConfiguration",
    fig = plotTimeProfile(
      data = simData,
      observedData = obsData,
      dataMapping = TimeProfileDataMapping$new(x = "x", y = "y",  ymin = "ymin", ymax = "ymax"),
      observedDataMapping = ObservedDataMapping$new(x = "x", y = "y", group = "group", shape = shape, color = shape,lloq = "lloq"),
      plotConfiguration = TimeProfilePlotConfiguration$new(lloqDirection = "horizontal", xAxisLimits = c(0,10), xValuesLimits = c(1,5))
    )
  )

})

