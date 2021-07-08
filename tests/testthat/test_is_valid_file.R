context("is_valid_file")

test_that("configuration file", {
  f <- system.file(
    "extdata", "sim_raster_data.yaml", package = "locationmisc")
  expect_true(isTRUE(is_valid_configuration_file(f)))
})

test_that("spatial file (raster)", {
  f <- system.file(
    "extdata", "sim_raster_spatial.tif", package = "locationmisc")
  expect_true(isTRUE(is_valid_spatial_file(f)))
})

test_that("spatial file (vector)", {
  f1 <- system.file(
    "extdata", "sim_vector_spatial.shp", package = "locationmisc")
  f2 <- system.file(
    "extdata", "sim_vector_spatial.dbf", package = "locationmisc")
  f3 <- system.file(
    "extdata", "sim_vector_spatial.shx", package = "locationmisc")
  f4 <- system.file(
    "extdata", "sim_vector_spatial.prj", package = "locationmisc")
  expect_true(isTRUE(is_valid_spatial_file(c(f1, f2, f3, f4))))
})

test_that("attribute file", {
  f <- system.file(
    "extdata", "sim_raster_attribute.csv.gz", package = "locationmisc")
  expect_true(isTRUE(is_valid_attribute_file(f)))
})

test_that("boundary file", {
  f <- system.file(
    "extdata", "sim_raster_boundary.csv.gz", package = "locationmisc")
  expect_true(isTRUE(is_valid_boundary_file(f)))
})
