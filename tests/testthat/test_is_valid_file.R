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
  # WGS84. is_valid_spatial_file output only TRUE with a projected CRS
  f4 <- system.file(
    "extdata", "projects", "sim_vector", "sim_vector_spatial.prj",
    package = "wheretowork"
  )
  expect_false(isTRUE(is_valid_spatial_file(c(f1, f2, f3, f4))))
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
