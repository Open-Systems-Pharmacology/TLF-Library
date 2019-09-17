# Auxiliary functions to automate the generation of data and metadata
getData <- function(size) {
  data <- data.frame(
    "IndividualID" = seq(1, size),
    "Population" = sample(c("Asian", "Caucasian", "African"), size = size, replace = TRUE),
    "Gender" = sample(c("M", "F"), size = size, replace = TRUE),
    "Age" = sample(seq(1, 30), size = size, replace = TRUE),
    "Compound" = sample(c("Aspirin", "Vancomycin"), size = size, replace = TRUE),
    "Dose" = sample(c(50, 100), size = size, replace = TRUE),
    "Organ" = rep("VenousBlood", size),
    "Compartment" = rep("Plasma", size),
    "Simulated" = sample(seq(4, 20, 0.1), size = size, replace = TRUE),
    "Observed" = sample(seq(4, 20, 0.1), size = size, replace = TRUE)
  )
  data$Ratio <- data$Simulated / data$Observed

  return(data)
}

getMetaData <- function(data) {
  metaData <- as.list(data)
  for (variable in colnames(data)) {
    metaData[[variable]] <- list("unit" = NULL, "dimension" = NULL, "variableType" = NULL)

    if (variable %in% "Age") {
      metaData[[variable]] <- list("unit" = "yrs", "dimension" = "Age", "variableType" = "numeric")
    }
    if (variable %in% "Time") {
      metaData[[variable]] <- list("unit" = "h", "dimension" = "Time", "variableType" = "numeric")
    }
    if (variable %in% "Dose") {
      metaData[[variable]] <- list("unit" = "mg", "dimension" = "Amount", "variableType" = "numeric")
    }
    if (variable %in% c("Observed", "Simulated", "Value")) {
      metaData[[variable]] <- list("unit" = "mg/L", "dimension" = "Concentration", "variableType" = "numeric")
    }
    if (variable %in% c("Ratio")) {
      metaData[[variable]] <- list("unit" = "", "dimension" = "Ratio", "variableType" = "numeric")
    }
  }
  return(metaData)
}
