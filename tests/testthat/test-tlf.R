# Get a few setting values for testing
maxCharacterWidth <- getTLFSettings(tlfSettingNames$maxCharacterWidth)$Value
alphaRatio <- getTLFSettings(tlfSettingNames$DefaultAlphaRatio)$Value
lloqLinetype <- getTLFSettings(tlfSettingNames$defaultLLOQLinetype)$Value

test_that("Changes of default settings work correctly", {
  setDefaultMaxCharacterWidth(100)
  expect_equal(getTLFSettings(tlfSettingNames$maxCharacterWidth)$Value, 100)
  
  setDefaultExportParameters(format = "pdf")
  expect_equal(getTLFSettings(tlfSettingNames$defaultExportParameters)$Value$format, "pdf")
  
  setDefaultAggregationFunctions(y = "mean")
  yFun <- getTLFSettings(tlfSettingNames$defaultAggregation)$Value$functions$y
  expect_type(yFun, "closure")
  expect_equal(yFun(1:10), mean(1:10))
  
  setDefaultAggregationLabels(y = "Mean")
  expect_equal(getTLFSettings(tlfSettingNames$defaultAggregation)$Value$labels$y, "Mean")
  
  setDefaultAggregationBins(5)
  expect_equal(getTLFSettings(tlfSettingNames$defaultAggregation)$Value$bins, 5)
  
  setDefaultLogTicks(1:10)
  expect_equal(getTLFSettings(tlfSettingNames$logTicks)$Value, 1:10)
  
  setDefaultErrorbarCapSize(2)
  expect_equal(getTLFSettings(tlfSettingNames$defaultErrorbarCapSize)$Value, 2)
  
  setDefaultLLOQLinetype("dashed")
  expect_equal(getTLFSettings(tlfSettingNames$defaultLLOQLinetype)$Value, "dashed")
  
  setDefaultAlphaRatio(0.5)
  expect_equal(getTLFSettings(tlfSettingNames$DefaultAlphaRatio)$Value, 0.5)
  
  setDefaultWatermark("My Watermark")
  expect_equal(getTLFSettings(tlfSettingNames$currentTheme)$Value$background$watermark, "My Watermark")
  
  setDefaultLegendPosition(LegendPositions$outsideBottom)
  expect_equal(getTLFSettings(tlfSettingNames$currentTheme)$Value$background$legendPosition, LegendPositions$outsideBottom)
  
  setDefaultLegendTitle("My Legend")
  expect_equal(getTLFSettings(tlfSettingNames$currentTheme)$Value$background$legendTitle, "My Legend")
})

test_that("Default settings can be saved", {
  saveTLFSettings("test-settings.RData")
  expect_true(file.exists("test-settings.RData"))
  
})

test_that("Default settings are reset to TLF Default", {
  resetTLFSettingsToDefault()
  expect_equal(getTLFSettings(tlfSettingNames$maxCharacterWidth)$Value, maxCharacterWidth)
  expect_equal(getTLFSettings(tlfSettingNames$DefaultAlphaRatio)$Value, alphaRatio)
  expect_equal(getTLFSettings(tlfSettingNames$defaultLLOQLinetype)$Value, lloqLinetype)
})

test_that("Previous default settings are reloaded", {
  loadTLFSettings("test-settings.RData")
  expect_equal(getTLFSettings(tlfSettingNames$maxCharacterWidth)$Value, 100)
  expect_equal(getTLFSettings(tlfSettingNames$DefaultAlphaRatio)$Value, 0.5)
  expect_equal(getTLFSettings(tlfSettingNames$defaultLLOQLinetype)$Value, "dashed")
})

# Clean up
resetTLFSettingsToDefault()
unlink("test-settings.RData")
