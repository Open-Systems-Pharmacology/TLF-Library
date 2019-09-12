context("getPKRatioMeasure")

test_that("getPKRatioMeasure works", {
  data <- data.frame("Ratio" = c(seq(0.1, 5, 0.1), NA), "OtherRatio" = c(seq(0.2, 9.8, 0.2), NA, NA))
  expectedDefaultResult <- data.frame(
    "Number" = c(50, 8, 16),
    "Ratio" = c(NA, 8 / 50, 16 / 50),
    row.names = c("PointsTotal", "Points within 1.5-fold", "Points within 2-fold")
  )
  expectedDefaultResult <- data.frame(
    "Number" = c(50, 8, 16),
    "Ratio" = c(NA, 8 / 50, 16 / 50),
    row.names = c("PointsTotal", "Points within 1.5-fold", "Points within 2-fold")
  )
  expectedResultLimit <- data.frame(
    "Number" = c(50, 26),
    "Ratio" = c(NA, 0.52),
    row.names = c("PointsTotal", "Points within 3-fold")
  )
  expectedResultMapping <- data.frame(
    "Number" = c(49, 4, 8),
    "Ratio" = c(NA, 4 / 49, 8 / 49),
    row.names = c("PointsTotal", "Points within 1.5-fold", "Points within 2-fold")
  )

  testMapping <- PKRatioDataMapping$new(y = "OtherRatio")

  # Check that return a data.frame
  expect_is(getPKRatioMeasure(data), "data.frame")
  # Check Default test
  expect_equal(getPKRatioMeasure(data), expectedDefaultResult)
  # Check change of Ratio Limits
  expect_equal(getPKRatioMeasure(data, dataMapping = NULL, ratioLimits = 3), expectedResultLimit)
  # Check change of DataMapping
  expect_equal(getPKRatioMeasure(data, dataMapping = testMapping), expectedResultMapping)
})
