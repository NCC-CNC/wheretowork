context("write_spatial_data")

test_that("vector data", {
  # create dataset
  d <- simulate_proportion_spatial_data(import_simple_vector_data(), 3)
  # save data
  f <- tempfile(fileext = ".zip")
  write_spatial_data(d, f)
  # run tests
  expect_true(file.exists(f))
  expect_true(
    all(
      paste0(tools::file_path_sans_ext(
        basename(f)
      ), c(".shp", ".prj", ".dbf", ".shx")) %in%
        unzip(f, list = TRUE)$Name
    )
  )
})

test_that("raster data", {
  # create dataset
  d <- simulate_proportion_spatial_data(import_simple_raster_data(), 3)
  # save data
  f <- tempfile(fileext = ".zip")
  write_spatial_data(d, f)
  # run tests
  expect_true(file.exists(f))
  expect_true(
    all(
      paste0(tools::file_path_sans_ext(basename(f)), c(".tif", ".txt")) %in%
        unzip(f, list = TRUE)$Name
    )
  )
})
