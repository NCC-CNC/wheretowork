context("simulate_themes")

test_that("simple dataset (single feature)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # themes are a mix of categorical and continuous defined by a conditional 
  x <- simulate_themes(d, 1, 1, continuous = NA)
  # run tests
  expect_is(x, "list")
  expect_length(x, 2)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) == 1, logical(1))),
    1
  )
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) > 1, logical(1))),
    1
  )
})


test_that("simple dataset (single feature, all categorical, manual legend)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # themes are all categorical
  x <- simulate_themes(d, 1, 1, continuous = FALSE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 2)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) == 1, logical(1))),
    1
  )
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) > 1, logical(1))),
    1
  )
})

test_that("simple dataset (multiple features)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # themes are a mix of categorical and continuous defined by a conditional 
  x <- simulate_themes(d, 3, 2, 2, continuous = NA)
  # run tests
  expect_is(x, "list")
  expect_length(x, 5)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) == 1, logical(1))),
    3
  )
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) > 1, logical(1))),
    2
  )
})

test_that("simple dataset (multiple features, all categorical, manual legend)", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  # themes are all categorical
  x <- simulate_themes(d, 3, 2, 2, continuous = FALSE)
  # run tests
  expect_is(x, "list")
  expect_length(x, 5)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) == 1, logical(1))),
    3
  )
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) > 1, logical(1))),
    2
  )
})

test_that("large dataset", {
  # create object
  d <- new_dataset_from_auto(import_realistic_raster_data())
  # themes are a mix of categorical and continuous defined by a conditional 
  x <- simulate_themes(d, 3, 2, 2, continuous = NA)
  # run tests
  expect_is(x, "list")
  expect_length(x, 5)
  expect_true(all_list_elements_inherit(x, "Theme"))
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) == 1, logical(1))),
    3
  )
  expect_equal(
    sum(vapply(x, function(x) length(x$feature) > 1, logical(1))),
    2
  )
})
