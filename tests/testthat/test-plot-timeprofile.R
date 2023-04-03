obsData <- data.frame(name = c("dataset1", "dataset1", "dataset2", "dataset2", "dataset2"),
                      x = c(1, 2, 1, 2, 3), y = c(5, 0.2, 2, 3, 4))

simTime <- seq(1, 10, 0.1)

simData <- data.frame(
 x = simTime,
 y = 10 * exp(-simTime),
 ymin = 8 * exp(-simTime),
 ymax = 12 * exp(-simTime)
)

color <- fill <- "group"
linetype <- shape <- "name"

plotTimeProfile(
 # data = simData,
 observedData = obsData,
 # dataMapping = TimeProfileDataMapping$new(x = "x", y = "y",  ymin = "ymin", ymax = "ymax"),
 observedDataMapping = ObservedDataMapping$new(x = "x", y = "y", shape = shape, color = shape)
)

