context("simulate spatial data")

test_that("proportion (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    terra::compareGeom(
      x, d
    )
  )
  expect_gte(min(terra::global(x, fun="min", na.rm=TRUE)[[1]]), 0)
  expect_lte(max(terra::global(x, fun="max", na.rm=TRUE)[[1]]), 1)
})

test_that("proportion (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_proportion_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), V1)), 1)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), V2)), 1)
})

test_that("continuous (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    terra::compareGeom(
      x, d
    )
  )
  expect_gte(min(terra::global(x, fun="min", na.rm=TRUE)[[1]]), 0)
})

test_that("continuous (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_continuous_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
})

test_that("categorical (raster)", {
  # create data
  d <- import_simple_raster_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "SpatRaster")
  expect_equal(terra::nlyr(x), 2)
  expect_equal(names(x), c("V1", "V2"))
  expect_true(
    terra::compareGeom(
      x, d
    )
  )
  expect_gte(min(terra::global(x, fun="min", na.rm=TRUE)[[1]]), 0)
  expect_lte(max(terra::global(x, fun="max", na.rm=TRUE)[[1]]), 11)
  expect_lte(n_distinct(c(terra::values(x))), 11)
})

test_that("categorical (sf)", {
  # create data
  d <- import_simple_vector_data()
  x <- simulate_categorical_spatial_data(d, 2)
  # run tests
  expect_is(x, "sf")
  expect_identical(sf::st_geometry(x), sf::st_geometry(d))
  expect_equal(names(x), c("V1", "V2", "geometry"))
  expect_gte(min(as.matrix(sf::st_drop_geometry(x))), 0)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), V1)), 11)
  expect_lte(max(dplyr::select(sf::st_drop_geometry(x), V2)), 11)
  expect_lte(n_distinct(dplyr::select(sf::st_drop_geometry(x), V1)), 11)
  expect_lte(n_distinct(dplyr::select(sf::st_drop_geometry(x), V2)), 11)
})
