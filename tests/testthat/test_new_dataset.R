context("new_dataset")

test_that("raster (from memory)", {
  # create object
  data(sim_features, package = "prioritizr")
  d <- sim_features
  x <- new_dataset(d)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, "memory")
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_variable(1), d[[1]])
  expect_identical(x$get_variable("layer.2"), d[["layer.2"]])
  expect_true(x$has_variable(3))
  expect_true(x$has_variable("layer.3"))
})

test_that("methods (sf from memory)", {
  # create object
  data(sim_pu_polygons, package = "prioritizr")
  d <- sim_pu_sf
  x <- new_dataset(d)
  # run tests
  expect_is(x$repr(), "character")
  expect_identical(x$source, "memory")
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_variable(1), d[, 1])
  expect_identical(x$get_variable("locked_in"), d[, "locked_in"])
  expect_true(x$has_variable(3))
  expect_true(x$has_variable("locked_out"))
})

test_that("raster (from file)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  f <- file.path(tempdir(), "layer.tif")
  suppressWarnings({
    raster::writeRaster(sim_features, f, NAflag = -9999, overwrite = TRUE)
  })
  d <- raster::stack(f)
  # create object
  x <- new_dataset(f)
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$source, f)
  expect_identical(x$data, NULL)
  x$import()
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_variable(1), d[[1]])
  expect_identical(x$get_variable("layer.2"), d[["layer.2"]])
  expect_true(x$has_variable(3))
  expect_true(x$has_variable("layer.3"))
})

test_that("methods (sf from file)", {
  # prepare data
  data(sim_pu_sf, package = "prioritizr")
  f <- tempfile(fileext = ".shp")
  suppressMessages({
    sf::write_sf(sim_pu_sf, f)
    d <- sf::read_sf(f)
  })
  # create object
  x <- new_dataset(f)
  # run tests
  expect_is(x$repr(), "character")
  expect_identical(x$source, f)
  expect_identical(x$data, NULL)
  x$import()
  expect_identical(x$data, d)
  expect_identical(x$get_data(), d)
  expect_identical(x$get_variable(1), d[, 1])
  expect_identical(x$get_variable("locked_in"), d[, "locked_in"])
  expect_true(x$has_variable(3))
  expect_true(x$has_variable("locked_out"))
})
