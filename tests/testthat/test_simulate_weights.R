context("simulate_weights")

test_that("simple dataset (single)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  x <- simulate_weights(d, 1)
  # run tests
  expect_is(x, "list")
  expect_length(x, 1)
  expect_true(all_list_elements_inherit(x, "Weight"))
})

test_that("simple dataset (multiple)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  x <- simulate_weights(d, 3)
  # run tests
  expect_is(x, "list")
  expect_length(x, 3)
  expect_true(all_list_elements_inherit(x, "Weight"))
})

test_that("large dataset", {
  # create object
  d <- new_dataset_from_auto(import_realistic_raster_data())
  x <- simulate_weights(d, 3)
  # run tests
  expect_is(x, "list")
  expect_true(all_list_elements_inherit(x, "Weight"))
})
