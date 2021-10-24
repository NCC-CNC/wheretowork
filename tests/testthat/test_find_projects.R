context("find_projects")

test_that("singe user group", {
  # create object
  d <- system.file("extdata", "projects", package = "wheretowork")
  x <- find_projects(d)
  # tests
  expect_is(x, "tbl_df")
  expect_named(x, c("path", "name", "status"))
  expect_equal(
    x$name,
    c("Ontario pilot dataset",
      "Example GeoTIFF dataset",
      "Example Shapefile dataset"
    )
  )
  expect_equal(
    basename(x$path),
    c("ontario_pilot.yaml",
      "sim_raster_data.yaml",
      "sim_vector_data.yaml"
    )
  )
  expect_equal(x$status, rep(TRUE, nrow(x)))
})

test_that("multiple user groups", {
  # create object
  d <- system.file("extdata", "projects", package = "wheretowork")
  x <- find_projects(d, c("public", "private"))
  # tests
  expect_is(x, "tbl_df")
  expect_named(x, c("path", "name", "status"))
  expect_equal(
    x$name,
    c(
      "Ontario pilot dataset",
      "Example GeoTIFF dataset",
      "Example GeoTIFF dataset (themes)",
      "Example GeoTIFF dataset (hidden)",
      "Example Shapefile dataset",
      "Example Shapefile dataset (themes)"
    )
  )
  expect_equal(
    basename(x$path),
    c("ontario_pilot.yaml",
      "sim_raster_data.yaml",
      "sim_raster2_data.yaml",
      "sim_raster3_data.yaml",
      "sim_vector_data.yaml",
      "sim_vector2_data.yaml"
    )
  )
  expect_equal(x$status, rep(TRUE, nrow(x)))
})
