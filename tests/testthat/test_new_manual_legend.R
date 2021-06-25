context("new_manual_legend")

test_that("initialization", {
  # create object
  x <- new_manual_legend(
    c("#000000", "#444444"), c("not selected", "selected"))
  # run tests
  expect_equal(x$colors, c("#000000", "#444444"))
  expect_equal(x$labels, c("not selected", "selected"))
})

test_that("widget method", {
  # create object
  x <- new_manual_legend(
    c("#000000", "#444444"), c("not selected", "selected"))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      values = c("not selected", "selected"),
      colors = x$colors,
      type = "CategoricalLegend"
    )
  )
})
