# The next line simply remove the variables from the environment
# Similar to clear from Matlab
rm(list = ls())

library(ggplot2)
library(tlf)

# Set your folder to test folder:
# setwd("C:/Design2Code/TLF-Playground/TLF-Playground/R/")
useTheme(defaultTheme)

# -------------------------------------------------
# Get the data and metadata for PK Ratio plot

nPopulation <- 20

# -------------------------------------------------

testData <- data.frame(
  IndivdualID = c(rep(1, 24), rep(2, 24)),
  Time = c(seq(1, 24), seq(1, 24)),
  Mean = c(10 * exp(-0.06 * seq(1, 24)), 10 * exp(-0.1 * seq(1, 24))),
  Min = c(5 * exp(-0.06 * seq(1, 24)), 8 * exp(-0.1 * seq(1, 24))),
  Max = c(15 * exp(-0.06 * seq(1, 24)), 12 * exp(-0.1 * seq(1, 24)))
)



testMetaData <- list(
  IndivdualID = list("unit" = "", "dimension" = ""),
  Time = list("unit" = "min", "dimension" = "Time"),
  Mean = list("unit" = "mg/L", "dimension" = "Concentration", "LLOQ" = 2),
  Min = list("unit" = "mg/L", "dimension" = "Concentration", "LLOQ" = 2.2),
  Max = list("unit" = "mg/L", "dimension" = "Concentration")
)

# -------------------------------------------------
# Define Default plot Configuration & Mapping from R6 class for PK Ratio
group <- GroupMapping$new(color = "IndivdualID")

lloqDataMapping <- TimeProfileDataMapping$new(
  x = "Time",
  y = "Mean",
  groupMapping = group
)

group <- GroupMapping$new(fill = "IndivdualID")
lloqRangeDataMapping <- TimeProfileDataMapping$new(
  x = "Time",
  ymin = "Min",
  ymax = "Max",
  groupMapping = group
)
# -------------------------------------------------
# Plot PK Ratio using the previously defined variables
lloqPlot <- plotTimeProfile(data = testData, metaData = testMetaData, dataMapping = lloqDataMapping)

lloqRangePlot <- plotTimeProfile(data = testData, metaData = testMetaData, dataMapping = lloqRangeDataMapping)


# -------------------------------------------------
set.seed(42)

obsData <- data.frame(x = rep(1:7, 2),
                      y = c(10 *exp(-1:-7) + rnorm(7,0,.25),10 * exp(-1:-7) + rnorm(7,0,.25)),
                      error = abs(rnorm(14, 0, 0.1)),
                      group = c(rep("A", 7), rep("B", 7)),
                      lloq = c(rep(0.15,7), rep(0.25, 7)))


obsDataMapping <- ObservedDataMapping$new(
  x = "x",
  y = "y",
  color = "group",
  error = "error",
  # lloq = "lloq"
)

simTime <- seq(1, 10, 0.1)
simData <- data.frame(
  x = simTime,
  y = 10 * exp(-simTime)
  # ymin = 8 * exp(-simTime),
  # ymax = 12 * exp(-simTime)
)

simDataMapping <- TimeProfileDataMapping$new(
  x = "x",
  y = "y"
  # ymin = "ymin",
  # ymax = "ymax"
)


plotTimeProfile(
  # data = simData, dataMapping = simDataMapping,
  observedData = obsData, observedDataMapping = obsDataMapping
)

#_________________________________________________________________________________
