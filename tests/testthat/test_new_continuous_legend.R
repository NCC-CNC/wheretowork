context("new_continuous_legend")

test_that("initialization", {
  # create object
  x <- new_continuous_legend(0, 100, c("#000000", "#444444", "#AAAAAA"))
  # run tests
  expect_equal(x$min_value, 0)
  expect_equal(x$max_value, 100)
  expect_equal(x$colors, c("#000000", "#444444", "#AAAAAA"))
})

test_that("widget method", {
  # create object
  x <- new_continuous_legend(0, 100, c("#000000", "#444444", "#AAAAAA"))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      min_value = 0,
      max_value = 100,
      colors = x$colors,
      type = "ContinuousLegend"
    )
  )
})
