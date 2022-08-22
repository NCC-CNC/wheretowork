context("new_null_legend")

test_that("initialization", {
  # create object
  x <- new_null_legend()
  # run tests
  expect_is(x, "NullLegend")
})

test_that("widget method", {
  # create object
  x <- new_null_legend()
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      type = "NullLegend"
    )
  )
})
