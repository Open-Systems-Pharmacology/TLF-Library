context("PK Ratio Plot")

# Load data of comprehensive examples
load("pkRatioDataExample.RData")

useTheme(tlfTheme)

test_that("plotPKRatio() function works properly", {
  pkrp <- plotPKRatio(
    data = pkRatioData,
    metaData = pkRatioMetaData
  )

  expect_is(pkrp, "ggplot")

  expect_error(
    plotPKRatio(),
    'argument "data" is missing, with no default'
  )
  expect_error(
    plotPKRatio(
      data = pkRatioData,
      dataMapping = list(
        x = "Age",
        y = "Ratio"
      )
    ),
    "argument 'dataMapping' is of type 'list', but expected 'PKRatioDataMapping'!"
  )
  expect_error(
    plotPKRatio(
      data = pkRatioData,
      plotConfiguration = list(title = "PK Ratio Plot")
    ),
    "argument 'plotConfiguration' is of type 'list', but expected 'PKRatioPlotConfiguration'!"
  )
})

test_that("PK Ratio default settings work", {
  pkRatioMapping <- PKRatioDataMapping$new()
  pkRatioConfig <- PKRatioPlotConfiguration$new()
  pkRatioConfigAsCalledInPlot <- PKRatioPlotConfiguration$new(
    data = pkRatioData,
    metaData = pkRatioMetaData,
    dataMapping = pkRatioMapping
  )

  expect_equal(pkRatioMapping$x, "Age")
  expect_equal(pkRatioMapping$y, "Ratio")
  expect_equal(pkRatioMapping$pkRatioLines, c(1, 1.5, 1 / 1.5, 2, 1 / 2))

  expect_is(pkRatioConfig$title, "Label")
  expect_is(pkRatioConfig$subtitle, "Label")
  expect_is(pkRatioConfig$xlabel, "Label")
  expect_is(pkRatioConfig$ylabel, "Label")

  expect_equal(pkRatioConfig$title$text, "PK Ratio Plot")
  expect_equal(pkRatioConfig$subtitle$text, paste("Date:", format(Sys.Date(), "%y-%m-%d")))
  expect_equal(pkRatioConfig$xlabel$text, "")
  expect_equal(pkRatioConfig$ylabel$text, "")
  expect_equal(pkRatioConfig$background$watermark$text, "tlf v0.1.0")

  expect_equal(pkRatioConfigAsCalledInPlot$title$text, "PK Ratio Plot")
  expect_equal(pkRatioConfigAsCalledInPlot$subtitle$text, paste("Date:", format(Sys.Date(), "%y-%m-%d")))
  expect_equal(pkRatioConfigAsCalledInPlot$xlabel$text, "Age [yrs]")
  expect_equal(pkRatioConfigAsCalledInPlot$ylabel$text, "Ratio")
  expect_equal(pkRatioConfigAsCalledInPlot$background$watermark$text, "tlf v0.1.0")
})

test_that("PK Ratio typical test works", {

  # tlf theme
  useTheme(tlfTheme)

  # Data mapping:
  # x = Age, y = Ratio, color = Gender, shape = c(Dose, Compound)
  pkRatioMap <- PKRatioDataMapping$new(
    x = "Age",
    y = "Ratio",
    color = "Gender",
    shape = c("Dose", "Compound")
  )

  # Use default configuration
  pkrp <- plotPKRatio(
    data = pkRatioData,
    metaData = pkRatioMetaData,
    dataMapping = pkRatioMap
  )

  expect_known_output(pkrp, "expectedPKRatioPlot.RData")
})
