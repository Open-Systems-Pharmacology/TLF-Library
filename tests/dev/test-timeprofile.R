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
  IndivdualID = rep(1, 24),
  Time = seq(1, 24),
  Value = 10 * exp(-0.06 * seq(1, 24))
)


testMetaData <- list(
  IndivdualID = list("unit" = "", "dimension" = ""),
  Time = list("unit" = "min", "dimension" = "Time"),
  Value = list("unit" = "mg/L", "dimension" = "Concentration")
)

# -------------------------------------------------
# Define Default plot Configuration & Mapping from R6 class for PK Ratio

testDataMapping <- TimeProfileDataMapping$new(x = "Time", y = "Value")

# Renaming of Label from initialize
testConfiguration <- TimeProfilePlotConfiguration$new(
  data = testData,
  metaData = testMetaData,
  dataMapping = testDataMapping
)

# -------------------------------------------------
# Plot PK Ratio using the previously defined variables
pkrp <- plotTimeProfile(
  data = testData, metaData = testMetaData,
  dataMapping = testDataMapping, plotConfiguration = testConfiguration
)
