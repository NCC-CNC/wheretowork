context("simulate_themes")

test_that("simple dataset (single)", {
  # create object
  d <- new_dataset(import_simple_raster_data())
  x <- simulate_themes(d, 1, 1)
  # run tests
  expect_is(x, "list")
  expect_length(x, 2)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "SingleTheme"),  logical(1))),
    1
  )
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "MultiTheme"),  logical(1))),
    1
  )
})

test_that("simple dataset (multiple)", {
  # create object
  d <- new_dataset(import_simple_raster_data())
  x <- simulate_themes(d, 2, 2, 2)
  # run tests
  expect_is(x, "list")
  expect_length(x, 4)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "SingleTheme"),  logical(1))),
    2
  )
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "MultiTheme"),  logical(1))),
    2
  )
})

test_that("large dataset", {
  # create object
  d <- new_dataset(import_realistic_raster_data())
  x <- simulate_themes(d, 2, 2, 2)
  # run tests
  expect_is(x, "list")
  expect_length(x, 4)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "SingleTheme"),  logical(1))),
    2
  )
  expect_equal(
    sum(vapply(x, function(x) inherits(x, "MultiTheme"),  logical(1))),
    2
  )
})
