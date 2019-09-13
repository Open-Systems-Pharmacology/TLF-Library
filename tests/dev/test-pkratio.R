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

testData <- getData(nPopulation)
testMetaData <- getMetaData(testData)

# -------------------------------------------------
# Define Default plot Configuration & Mapping from R6 class for PK Ratio
testDataMapping <- PKRatioDataMapping$new(colorGrouping = "Gender", shapeGrouping = c("Dose", "Compound"))

# Renaming of Label from initialize
testConfiguration <- PKRatioPlotConfiguration$new(
  data = testData,
  metaData = testMetaData,
  dataMapping = testDataMapping
)

# -------------------------------------------------
# Plot PK Ratio using the previously defined variables
pkrp <- plotPKRatio(
  data = testData, metaData = testMetaData,
  dataMapping = testDataMapping, plotConfiguration = testConfiguration
)

# -------------------------------------------------
# Test of getPKRatioMeasure
PKRatioMeasure <- getPKRatioMeasure(data = testData, dataMapping = testDataMapping)

print(PKRatioMeasure)

# Saving of plot
# testConfiguration$savePlot(pkrp)
