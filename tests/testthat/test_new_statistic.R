context("new_statistic")

test_that("initialization", {
  # create object
  x <- new_statistic(name = "Area", value = 12, units = "ha")
  # run tests
  expect_is(x, "Statistic")
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "Area")
  expect_identical(x$value, 12)
  expect_identical(x$units, "ha")
})

test_that("widget method", {
  # create object
  x <- new_statistic(name = "Area", value = 12, units = "ha")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      name = x$name,
      value = x$value,
      units = x$units
    )
  )
})
