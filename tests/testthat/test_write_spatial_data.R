context("write_spatial_data")

test_that("vector data", {
  # create dataset
  d <- simulate_proportion_spatial_data(import_simple_vector_data(), 3)
  # save data
  write_spatial_data(d, dir = tempdir(), name = "data")
  # run tests
  expect_true(
    all(
      file.exists(
        file.path(tempdir(), paste0("data", c(".shp", ".prj", ".dbf", ".shx")))
      )
    )
  )
})

test_that("raster data", {
  # create dataset
  d <- simulate_proportion_spatial_data(import_simple_raster_data(), 3)
  # convert to SpatRaster
  d <- terra::rast(d)
  # save data
  write_spatial_data(d, dir = tempdir(), name = "data2")
  # run tests
  expect_true(
    all(
      file.exists(
        file.path(tempdir(), paste0("data2", c(".tif", ".txt")))
      )
    )
  )
})
