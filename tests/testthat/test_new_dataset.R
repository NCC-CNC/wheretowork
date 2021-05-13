context("new_dataset")

test_that("initialization", {
  # create object
  l <- simulate_continuous_legend()
  x <- new_dataset(
    source = "asdf.tif", total = 200, units = "ha", legend = l)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, "asdf.tif")
  expect_identical(x$total, 200)
  expect_identical(x$units, "ha")
  expect_equal(x$legend, l)
})
