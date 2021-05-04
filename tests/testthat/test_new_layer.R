context("new_layer")

test_that("initialization", {
  # create object
  x <- new_layer(source = "asdf.tif", current = 0.2, total = 200, units = "ha")
  # run tests
  expect_identical(x$source, "asdf.tif")
  expect_identical(x$current, 0.2)
  expect_identical(x$total, 200)
  expect_identical(x$units, "ha")
})
