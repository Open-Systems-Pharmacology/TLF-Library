# The next line simply remove the variables from the environment
# Similar to clear from Matlab
rm(list = ls())

library(ggplot2)
library(tlf)

# Set your folder to test folder:
# setwd("C:/Design2Code/TLF-Playground/TLF-Playground/R/")
useTheme(bigTheme)

# -------------------------------------------------
# Get the data and metadata for PK Ratio plot

nPopulation <- 20

# -------------------------------------------------

testData <- list(
  observedData = data.frame(
    StudyID = c(1, 1, 1),
    Time = c(1, 10, 24),
    Value = c(10, 5, 2.5),
    Error = c(2, 1, 0.5)
  ),
  simulatedResults = data.frame(
    IndivdualID = rep(1, 24),
    Time = seq(1, 24),
    Value = 10 * exp(-0.06 * seq(1, 24))
  )
)

testMetaData <- list()
testMetaData$observedData <- getMetaData(testData$observedData)
testMetaData$simulatedResults <- getMetaData(testData$simulatedResults)
testMetaData$observedData$Value <- list(unit = "mg/L", dimension = "Concentration")
testMetaData$simulatedResults$Value <- list(unit = "mg/L", dimension = "Concentration")



# -------------------------------------------------
# Define Default plot Configuration & Mapping from R6 class for PK Ratio
testObservationMapping <- XYEDataMapping$new(x = "Time", y = "Value", error = "Error")

testDataMapping <- TimeProfileDataMapping$new(
  simulationSets = "simulatedResults",
  observationSets = "observedData",
  observationMapping = testObservationMapping
)

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
