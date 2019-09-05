# The next line simply remove the variables from the environment
# Similar to clear from Matlab
rm(list = ls())

library(ggplot2)
library(tlf)

# Set your folder to test folder:
# setwd("C:/Design2Code/TLF-Playground/TLF-Playground/R/")


# -------------------------------------------------
# Example of definition of  Data for Cmax Ratio plot
# Hopefully, performed previoulsy by the OSP Suite

CmaxRatio.Data <- data.frame(
  "IndividualID" = c(1, 2, 3),
  "Population" = rep("Asian", 3),
  "Gender" = as.factor(c("M", "F", "F")),
  "Age" = c(1, 2, 3),
  "Compound" = rep("Aspirin", 3),
  "Dose" = c(2, 3, 4),
  "Organ" = rep("VenousBlood", 3),
  "Compartment" = rep("Plasma", 3),
  "Simulated" = c(12, 9, 5),
  "Observed" = c(10, 8, 5)
)

# TO BE DISCUSSED
# The variable "Ratio" and its metadata can be calculated through the OSP-Library
# But currently, they are implmented in this test script
CmaxRatio.Data$Ratio <- CmaxRatio.Data$Simulated / CmaxRatio.Data$Observed

# Save/Load Data to/from a csv
# write.csv(CmaxRatio.Data, 'CmaxRatio_Data.csv', row.names = FALSE)
# CmaxRatio.Data2 <- read.csv('CmaxRatio_Data.csv')

# -------------------------------------------------
# Example of definition of MetaData
# Hopefully, performed previoulsy by the OSP Suite
CmaxRatio.MetaData <- list(
  "IndividualID" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "numeric"),
  "Population" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "character"),
  "Gender" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "logical"),
  "Age" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "numeric"),
  "Compound" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "character"),
  "Dose" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "numeric"),
  "Organ" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "character"),
  "Compartment" = list("Unit" = NA, "Dimension" = NA, "VariableType" = "character"),
  "Simulated" = list("Unit" = "g/mL", "Dimension" = "Concentration", "VariableType" = "numeric"),
  "Observed" = list("Unit" = "g/mL", "Dimension" = "Concentration", "LLOQ" = 2.0, "VariableType" = "numeric") # ,
  # "Ratio" = list("Unit"=NULL, "Dimension"="Fraction", "VariableType"="numeric")
)
# Create GMFE as a MetaData for Ratio
CmaxRatio.MetaData$Ratio$GMFE <- 10^(mean(abs(log10(CmaxRatio.Data$Ratio))))

# Save/Load MetaData to/from a json
# cat(jsonlite::toJSON(CmaxRatio.MetaData, pretty = TRUE, auto_unbox = TRUE), file = "CmaxRatio_MetaData.json")
# CmaxRatio.MetaData2 <- jsonlite::fromJSON("CmaxRatio_MetaData.json")


# -------------------------------------------------
# Define Configuration & Mapping from R6 class
CmaxRatio.Configuration <- PKRatioPlotConfiguration$new()
# Example with a different theme changing
CmaxRatio.Configuration$setTLFTheme()
CmaxRatio.Configuration$setBWFont()
CmaxRatio.DataMapping <- PKRatioDataMapping$new(colorGrouping = "Gender", shapeGrouping = "Compound")

# -------------------------------------------------
# Test example of previous blocks
pkrp <- plotPKRatio(
  data = CmaxRatio.Data, metaData = CmaxRatio.MetaData,
  dataMapping = CmaxRatio.DataMapping, plotConfiguration = CmaxRatio.Configuration
)

# Save plot as a specific format
# ggsave(filename = 'C:/Design2Code/TLF-Playground/TLF-Playground/R/TestPKRatio.png', plot = pkrp, width = 20, height = 10, units = "cm")
