context("asLabel")

test_that("asLabel gives always a Label class object", {
  x <- asLabel("a")
  expect_is(x, "Label")
  expect_is(asLabel(x), "Label")
})
