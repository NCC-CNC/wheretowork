context("new_null_legend")

test_that("initialization", {
  # create object
  x <- new_null_legend()
  # run tests
  expect_equal(x$values, NULL)
  expect_equal(x$colors, NULL)
})

test_that("widget method", {
  # create object
  x <- new_null_legend()
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      values = NULL,
      colors = NULL,
      type = "NullLegend"
    )
  )
})
