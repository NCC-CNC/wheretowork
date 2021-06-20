context("new_categorical_legend")

test_that("initialization", {
  # create object
  x <- new_categorical_legend(c(0, 1, 2), c("#000000", "#444444", "#AAAAAA"))
  # run tests
  expect_equal(x$values, c(0, 1, 2))
  expect_equal(x$colors, c("#000000", "#444444", "#AAAAAA"))
})

test_that("export method", {
  # create object
  x <- new_categorical_legend(c(0, 1, 2), c("#000000", "#444444", "#AAAAAA"))
  # run tests
  expect_equal(
    x$export(),
    list(
      type = "categorical",
      colors = c("#000000", "#444444", "#AAAAAA")
    )
  )
})

test_that("widget method", {
  # create object
  x <- new_categorical_legend(c(0, 1, 2), c("#000000", "#444444", "#AAAAAA"))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      values = x$values,
      colors = x$colors,
      type = "CategoricalLegend"
    )
  )
})
