# The next line simply remove the variables from the environment
# Similar to clear from Matlab
rm(list = ls())

library(ggplot2)
library(tlf)

# Set your folder to test folder:
# setwd("C:/Design2Code/TLF-Playground/TLF-Playground/R/")

# -------------------------------------------------
# Get the data and metadata for PK Ratio plot

nPopulation <- 20

# -------------------------------------------------

testData <- getData(nPopulation)
testMetaData <- getMetaData(testData)

# Grouping works through dataMapping that calls the function getDefaultCaption
# Below is an example,
# Abdullah's code to be fully implemented to improve the following function:

print(getDefaultCaptions(testData, testMetaData, variableList = c("Age", "Dose", "Compound")))

# Without argument, take all the variables
print(getDefaultCaptions(testData, testMetaData))