context("new_statistic")

test_that("initialization", {
  # create object
  x <- new_statistic(name = "Area", value = 12, units = "ha", proportion = 0.2)
  # run tests
  expect_is(x, "Statistic")
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "Area")
  expect_identical(x$value, 12)
  expect_identical(x$units, "ha")
  expect_identical(x$proportion, 0.2)
  expect_is(x$get_results_data(), "data.frame")
})

test_that("widget method", {
  # create object
  x <- new_statistic(name = "Area", value = 12, units = "ha", proportion = 0.2)
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      name = "Area",
      value = 12,
      units = "ha",
      proportion = 0.2
    )
  )
})

test_that("NA_real_ values", {
  # create object
  x <- new_statistic(name = "Area", value = NA_real_, units = "ha", proportion = 0.2)
  # run tests
  expect_is(x, "Statistic")
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "Area")
  expect_equal(is.na(x$value), TRUE)
  expect_identical(x$units, "ha")
  expect_identical(x$proportion, 0.2)
  expect_is(x$get_results_data(), "data.frame")
})
