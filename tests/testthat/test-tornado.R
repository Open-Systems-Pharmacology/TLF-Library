# Create a data.frames for the plot tests
tornadoPopData <- data.frame(
  sensitivity = c(c(1, 2, 3, -1, -2, -3), c(1, 2, 3, -1, -2, -3) - 0.2),
  path = rep(rep(c("liver", "kidney"), each = 3), 2),
  quantile = rep(rep(c("5th", "50th", "95th"), 2), 2),
  population = rep(c("Adults", "Peds"), each = 6)
)

tornadoMeanData <- tornadoPopData[tornadoPopData$population %in% "Adults" &
  tornadoPopData$quantile %in% "50th", ]

test_that("TornadoDataMapping features", {
  dataMapping0 <- TornadoDataMapping$new(x = "sensitivity", y = "path")
  dataMappingWithOption <- TornadoDataMapping$new(x = "sensitivity", y = "path", sorted = FALSE)

  dataMappingColor <- TornadoDataMapping$new(x = "sensitivity", y = "path", color = "path")

  dataMappingShape <- TornadoDataMapping$new(
    x = "sensitivity", y = "path",
    color = "population", shape = "quantile"
  )

  # snapshot tests ------------------------
  expect_snapshot(list(
    dataMapping0, dataMappingWithOption,
    dataMappingColor, dataMappingShape
  ))

  # test behavior of objects ---------------
  expect_equal(dataMapping0$x, "sensitivity")
  expect_equal(dataMapping0$y, "path")
  expect_true(dataMapping0$sorted)

  expect_equal(dataMappingWithOption$x, "sensitivity")
  expect_equal(dataMappingWithOption$y, "path")
  expect_false(dataMappingWithOption$sorted)

  expect_equal(dataMappingColor$x, "sensitivity")
  expect_equal(dataMappingColor$y, "path")
  # For tornado, if not specified, fill, shape and color properties are linked
  # to get directly a correct plot
  expect_equal(dataMappingColor$groupMapping$color$group, "path")
  expect_equal(dataMappingColor$groupMapping$fill$group, "path")
  expect_equal(dataMappingColor$groupMapping$shape$group, "path")

  expect_equal(dataMappingShape$x, "sensitivity")
  expect_equal(dataMappingShape$y, "path")
  # Since it is specified, shape variable is different here
  expect_equal(dataMappingShape$groupMapping$color$group, "population")
  expect_equal(dataMappingShape$groupMapping$fill$group, "population")
  expect_equal(dataMappingShape$groupMapping$shape$group, "quantile")
})

test_that("Regular tornado plots with their options work properly", {
  skip_on_cran()
  skip_if_not_installed("vdiffr")
  skip_if_not(getRversion() >= "4.1")

  # Direct Tornado Plot
  set.seed(123)
  defaultPlot <- plotTornado(x = tornadoMeanData$sensitivity, y = tornadoMeanData$path)
  vdiffr::expect_doppelganger(
    title = "defaultPlot",
    fig = defaultPlot
  )

  # Options/Features
  set.seed(123)
  unsortedPlot <- plotTornado(x = tornadoMeanData$sensitivity, y = tornadoMeanData$path, sorted = FALSE)
  vdiffr::expect_doppelganger(
    title = "unsortedPlot",
    fig = unsortedPlot
  )

  set.seed(123)
  colorPalettePlot <- plotTornado(x = tornadoMeanData$sensitivity, y = tornadoMeanData$path, colorPalette = "Dark2")
  vdiffr::expect_doppelganger(
    title = "colorPalettePlot",
    fig = colorPalettePlot
  )

  set.seed(123)
  pointPlot <- plotTornado(x = tornadoMeanData$sensitivity, y = tornadoMeanData$path, bar = FALSE)
  vdiffr::expect_doppelganger(
    title = "pointPlot",
    fig = pointPlot
  )

  # Higher level with dataMapping used in population sensitivity plots
  # bar is an input option that can be directly provided to plotConfig input
  set.seed(123)
  popSensitivityPlot <- plotTornado(
    data = tornadoPopData,
    bar = FALSE,
    dataMapping = TornadoDataMapping$new(
      x = "sensitivity",
      y = "path",
      color = "quantile",
      shape = "population"
    )
  )
  vdiffr::expect_doppelganger(
    title = "popSensitivityPlot",
    fig = popSensitivityPlot
  )
})
