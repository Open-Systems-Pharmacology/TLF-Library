context("Font and label")

test_that("asLabel gives always a Label class object", {
  x <- asLabel("a")
  expect_is(x, "Label")
  expect_is(asLabel(x), "Label")
})

test_that("Font default works", {
  defaultFont <- Font$new()

  expect_is(defaultFont, "Font")
  expect_is(defaultFont$size, "numeric")
  expect_is(defaultFont$angle, "numeric")
  expect_is(defaultFont$color, "character")
  expect_is(defaultFont$fontFace, "character")
  expect_is(defaultFont$fontFamily, "character")
})

test_that("Label default works", {
  defaultLabel <- Label$new()

  expect_is(defaultLabel, "Label")
  expect_is(defaultLabel$text, "character")
  expect_is(defaultLabel$font, "Font")
})

test_that("Label gives error when initialized with wrong arguments", {
  expect_error(Label$new(font = ""))
  expect_error(Font$new(color = 3))
  expect_error(Font$new(size = "3"))
})
