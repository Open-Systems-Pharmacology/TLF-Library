context("Plot Configuration")

# Create a reference plot configuration
refPlotConfiguration <- PlotConfiguration$new()
for (property in AestheticProperties) {
  refPlotConfiguration$lines[[property]] <- "reference"
  refPlotConfiguration$ribbons[[property]] <- "reference"
  refPlotConfiguration$points[[property]] <- "reference"
  refPlotConfiguration$errorbars[[property]] <- "reference"
}

test_that("Theme properties are not updated when PlotConfiguration is updated", {
  for (property in AestheticProperties) {
    testPlotConfiguration <- PlotConfiguration$new()
    expect_false(testPlotConfiguration$lines[[property]] %in% "reference")
    expect_false(testPlotConfiguration$ribbons[[property]] %in% "reference")
    expect_false(testPlotConfiguration$points[[property]] %in% "reference")
    expect_false(testPlotConfiguration$errorbars[[property]] %in% "reference")
  }
})
