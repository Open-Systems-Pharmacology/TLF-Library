# Test data

set.seed(42)

obsData <- data.frame(
  x = rep(1:7, 2),
  y = c(10 * exp(-1:-7) + rnorm(7, 0, .25), 10 * exp(-1:-7) + rnorm(7, 0, .25)),
  error = abs(rnorm(14, 0, 0.1)),
  group = c(rep("A", 7), rep("B", 7)),
  lloq = c(rep(0.15, 7), rep(0.25, 7))
)

simTime <- seq(1, 10, 0.1)
simData <- data.frame(
  x = simTime,
  y = 10 * exp(-simTime),
  ymin = 8 * exp(-simTime),
  ymax = 12 * exp(-simTime)
)


# Basic plot

test_that("plotTimeProfile works ", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")

  obsDataMapping <- ObservedDataMapping$new(
    x = "x",
    y = "y"
  )

  simDataMapping <- TimeProfileDataMapping$new(
    x = "x",
    y = "y"
  )

  vdiffr::expect_doppelganger(
    title = "basic",
    fig = plotTimeProfile(
      data = simData,
      dataMapping = simDataMapping,
      observedData = obsData,
      observedDataMapping = obsDataMapping
    )
  )
})

# LLOQ plots


test_that("plotTimeProfile works with LLOQ ", {
  skip_if_not_installed("vdiffr")
  skip_if(getRversion() < "4.1")
  vdiffr::expect_doppelganger(
    title = "with observed data only",
    fig = plotTimeProfile(
      observedData = obsData,
      observedDataMapping = ObservedDataMapping$new(
        x = "x",
        y = "y",
        group = "group",
        lloq = "lloq"
      )
    )
  )

  vdiffr::expect_doppelganger(
    title = "with observed and sim data",
    fig = plotTimeProfile(
      observedData = obsData,
      observedDataMapping = ObservedDataMapping$new(
        x = "x",
        y = "y",
        group = "group",
        lloq = "lloq"
      ),
      data = simData,
      dataMapping = TimeProfileDataMapping$new(
        x = "x",
        y = "y"
      )
    )
  )
})



# Legacy tests

# -------------------------------------------------
# Get the data and metadata for PK Ratio plot

# nPopulation <- 20
#
# # -------------------------------------------------
#
# testData <- data.frame(
#   IndivdualID = c(rep(1, 24), rep(2, 24)),
#   Time = c(seq(1, 24), seq(1, 24)),
#   Mean = c(10 * exp(-0.06 * seq(1, 24)), 10 * exp(-0.1 * seq(1, 24))),
#   Min = c(5 * exp(-0.06 * seq(1, 24)), 8 * exp(-0.1 * seq(1, 24))),
#   Max = c(15 * exp(-0.06 * seq(1, 24)), 12 * exp(-0.1 * seq(1, 24)))
# )
#
#
#
# testMetaData <- list(
#   IndivdualID = list("unit" = "", "dimension" = ""),
#   Time = list("unit" = "min", "dimension" = "Time"),
#   Mean = list("unit" = "mg/L", "dimension" = "Concentration", "LLOQ" = 2),
#   Min = list("unit" = "mg/L", "dimension" = "Concentration", "LLOQ" = 2.2),
#   Max = list("unit" = "mg/L", "dimension" = "Concentration")
# )
#
# # -------------------------------------------------
# # Define Default plot Configuration & Mapping from R6 class for PK Ratio
# group <- GroupMapping$new(color = "IndivdualID")
#
# lloqDataMapping <- TimeProfileDataMapping$new(
#   x = "Time",
#   y = "Mean",
#   groupMapping = group
# )
#
# group <- GroupMapping$new(fill = "IndivdualID")
# lloqRangeDataMapping <- TimeProfileDataMapping$new(
#   x = "Time",
#   ymin = "Min",
#   ymax = "Max",
#   groupMapping = group
# )
# # -------------------------------------------------
# # Plot PK Ratio using the previously defined variables
# lloqPlot <- plotTimeProfile(data = testData, metaData = testMetaData, dataMapping = lloqDataMapping)
#
# lloqRangePlot <- plotTimeProfile(data = testData, metaData = testMetaData, dataMapping = lloqRangeDataMapping)
