context("asLabel")

test_that("asLabel works", {
  x <- asLabel("a")
  # Check for class of output
  expect_is(x, "Label")
  # Check that asLabel of Label is still Label class
  expect_is(asLabel(x), "Label")
})
