context("is_valid_file")

test_that("configuration file", {
  f <- system.file(
    "extdata", "projects", "sim_raster", "sim_raster_data.yaml",
    package = "wheretowork"
  )
  expect_true(isTRUE(is_valid_configuration_file(f)))
})

test_that("spatial file (raster)", {
  f <- system.file(
    "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
    package = "wheretowork"
  )
  expect_true(isTRUE(is_valid_spatial_file(f)))
})

test_that("spatial file (vector)", {
  f1 <- system.file(
    "extdata", "projects", "sim_vector", "sim_vector_spatial.shp",
    package = "wheretowork"
  )
  f2 <- system.file(
    "extdata", "projects", "sim_vector", "sim_vector_spatial.dbf",
    package = "wheretowork"
  )
  f3 <- system.file(
    "extdata", "projects", "sim_vector", "sim_vector_spatial.shx",
    package = "wheretowork"
  )
  f4 <- system.file(
    "extdata", "projects", "sim_vector", "sim_vector_spatial.prj",
    package = "wheretowork"
  )
  # note that data are in WGS84 (longitude/latitude)
  expect_true(isTRUE(is_valid_spatial_file(c(f1, f2, f3, f4))))
})

test_that("spatial file (raster, longitude/latitude)", {
  # create data
  r <- terra::rast(
    nrows = 2, ncols = 3, xmin = 0, xmax = 3, ymin = 50, ymax = 52,
    crs = "EPSG:4326", vals = seq_len(6)
  )
  f <- tempfile(fileext = ".tif")
  terra::writeRaster(r, f)
  # run tests
  expect_true(isTRUE(is_valid_spatial_file(f)))
  # clean up
  unlink(f)
})

test_that("spatial file (raster, missing CRS)", {
  # create data
  ## note that extent is outside longitude/latitude range, because
  ## otherwise terra assumes a geographic CRS when reading the data
  r <- terra::rast(
    nrows = 2, ncols = 3, xmin = 0, xmax = 300, ymin = 0, ymax = 200,
    crs = "", vals = seq_len(6)
  )
  f <- tempfile(fileext = ".tif")
  suppressWarnings(terra::writeRaster(r, f))
  # run tests
  expect_identical(
    is_valid_spatial_file(f),
    "Error: coordinate reference system must be defined"
  )
  # clean up
  unlink(f)
})

test_that("attribute file", {
  f <- system.file(
    "extdata", "projects", "sim_raster", "sim_raster_attribute.csv.gz",
    package = "wheretowork"
  )
  expect_true(isTRUE(is_valid_attribute_file(f)))
})

test_that("boundary file", {
  f <- system.file(
    "extdata", "projects", "sim_raster", "sim_raster_boundary.csv.gz",
    package = "wheretowork"
  )
  expect_true(isTRUE(is_valid_boundary_file(f)))
})
