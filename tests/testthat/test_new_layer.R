context("new_layer")

test_that("initialization", {
  # create object
  x <- new_layer(source = "asdf.tif", total = 200, units = "ha")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, "asdf.tif")
  expect_identical(x$total, 200)
  expect_identical(x$units, "ha")
})
