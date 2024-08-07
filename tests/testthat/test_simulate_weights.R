context("simulate_weights")

test_that("simple dataset (single, continuous)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # weights are continuous 
  x <- simulate_weights(d, 1, continuous = TRUE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 1)
  expect_true(all_list_elements_inherit(x, "Weight"))
})

test_that("simple dataset (multiple, continuous)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # weights are continuous
  x <- simulate_weights(d, 3, continuous = TRUE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 3)
  expect_true(all_list_elements_inherit(x, "Weight"))
})

test_that("simple dataset (single, catagorical)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # weights are categorical 
  x <- simulate_weights(d, 1, continuous = FALSE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 1)
  expect_true(all_list_elements_inherit(x, "Weight"))
})

test_that("simple dataset (multiple, catagorical)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # weights are categorical 
  x <- simulate_weights(d, 3, continuous = FALSE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 3)
  expect_true(all_list_elements_inherit(x, "Weight"))
})


test_that("large dataset", {
  # create object
  d <- new_dataset_from_auto(import_realistic_raster_data())
  # weights are continuous
  x <- simulate_weights(d, 3, continuous = TRUE)
  # run tests
  expect_is(x, "list")
  expect_true(all_list_elements_inherit(x, "Weight"))
})
