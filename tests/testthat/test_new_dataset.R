context("new_dataset")

test_that("raster (from memory)", {
  # create object
  d <- simulate_proportion_spatial_data(import_simple_raster_data(), 3)
  x <- new_dataset(d)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, "memory")
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_index(1), d[[1]])
  expect_identical(x$get_index(names(d)[[2]]), d[[names(d)[[2]]]])
  expect_true(x$has_index(3))
  expect_true(x$has_index(names(d)[[3]]))
  expect_false(x$has_index("ASDFG"))
})

test_that("methods (sf from memory)", {
  # create object
  d <- simulate_proportion_spatial_data(import_simple_vector_data(), 3)
  x <- new_dataset(d)
  # run tests
  expect_is(x$repr(), "character")
  expect_identical(x$source, "memory")
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_index(names(d)[[2]]), d[, names(d)[[2]]])
  expect_true(x$has_index(3))
  expect_true(x$has_index(names(d)[[3]]))
  expect_false(x$has_index("ASDFG"))
})

test_that("raster (from file)", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- readNamedRaster(f)
  x <- new_dataset(f)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, f)
  expect_identical(x$data, NULL)
  x$import()
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_index(1), d[[1]])
  expect_identical(x$get_index(names(d)[[2]]), d[[names(d)[[2]]]])
  expect_true(x$has_index(3))
  expect_true(x$has_index(names(d)[[3]]))
  expect_false(x$has_index("ASDFG"))
})

test_that("methods (sf from file)", {
  # create object
  f <- system.file("extdata", "sim_vector_data.gpkg", package = "locationmisc")
  d <- suppressMessages(sf::read_sf(f))
  x <- new_dataset(f)
  # run tests
  expect_is(x$repr(), "character")
  expect_identical(x$source, f)
  expect_identical(x$data, NULL)
  x$import()
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_index(1), d[, 1])
  expect_identical(x$get_index(names(d)[[2]]), d[, names(d)[[2]]])
  expect_true(x$has_index(3))
  expect_true(x$has_index(names(d)[[3]]))
  expect_false(x$has_index("ASDFG"))
})
