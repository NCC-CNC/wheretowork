context("simulate spatial data")

test_that("proportion (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
  expect_lte(max(raster::cellStats(x, "max")), 1)
})

test_that("proportion (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "id", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V1), 1)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V2), 1)
})

test_that("continuous (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
})

test_that("continuous (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "id", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
})

test_that("categorical (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "Raster")
  expect_equal(raster::nlayers(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    raster::compareRaster(
      x, d,
      res = TRUE, tolerance = 1e-5, stopiffalse = FALSE
    )
  )
  expect_gte(min(raster::cellStats(x, "min")), 0)
  expect_lte(max(raster::cellStats(x, "max")), 11)
  expect_lte(n_distinct(c(raster::values(x))), 11)
})

test_that("categorical (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "id", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V1), 11)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V2), 11)
  expect_lte(n_distinct(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V1), 11)
  expect_lte(n_distinct(dplyr::select(sf::st_drop_geometry(x), -c("id"))$V2), 11)
})
