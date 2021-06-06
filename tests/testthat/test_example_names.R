context("example names")

test_that("example_weight_names", {
  # create object
  x <- example_weight_names()
  # run tests
  expect_is(x, "data.frame")
  expect_gt(nrow(x), 0)
  expect_is(x$name, "character")
  expect_is(x$unit, "character")
  expect_identical(anyDuplicated(x$name), 0L)
})

test_that("example_theme_names", {
  # create object
  x <- example_theme_names()
  # run tests
  expect_is(x, "data.frame")
  expect_gt(nrow(x), 0)
  expect_is(x$feature, "character")
  expect_is(x$theme, "character")
  expect_identical(anyDuplicated(x$feature), 0L)
  expect_gt(n_distinct(x$theme), 100)
})
