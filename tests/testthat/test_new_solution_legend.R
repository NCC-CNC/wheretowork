context("new_solution_legend")

test_that("initialization", {
  # create object
  x <- new_solution_legend(c("#000000", "#444444"))
  # run tests
  expect_equal(x$colors, c("#000000", "#444444"))
})

test_that("widget method", {
  # create object
  x <- new_solution_legend(c("#000000", "#444444"))
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
