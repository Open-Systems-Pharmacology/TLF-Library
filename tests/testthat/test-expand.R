# To prevent copy/paste, expressions are used in the sequel
plotNames <- c("DDIRatio", "", "PKRatio", "TimeProfile", "ObsVsPred", "ResVsPred", "Histogram", "BoxWhisker", "Tornado")

# Epression is "<plotname>plotConfiguration <- <PlotName>PlotConfiguration$new()"
createPlotConfigurations <- parse(text = paste0(tolower(plotNames), "plotConfiguration <- ", plotNames, "PlotConfiguration$new()"))
eval(createPlotConfigurations)

# Epression is "expect_false(<plotname>plotConfiguration$defaultExpand)"
# Use -1 to remove first element ie DDI Ratio from the evaluation
checkExpandIsFalse <- parse(text = paste0("expect_false(", tolower(plotNames[-1]), "plotConfiguration$defaultExpand)"))

test_that("Default expand is true for DDI ratio only", {
  eval(checkExpandIsFalse)
  expect_true(ddiratioplotConfiguration$defaultExpand)
})
