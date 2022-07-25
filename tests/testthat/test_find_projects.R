context("find_projects")

test_that("singe user group", {
  # create object
  d <- system.file("extdata", "projects", package = "wheretowork")
  x <- find_projects(d)
  # arrange tibble name by alphabetical order (needed for github actions)
  x <-  dplyr::arrange(x, name)
  # tests
  expect_is(x, "tbl_df")
  expect_named(x, c("path", "name", "status"))
  expect_equal(
    x$name,
    c("Example GeoTIFF dataset",
      "Example Shapefile dataset",
      "Ontario pilot dataset",
      "Ontario pilot dataset Albers",
      "Ontario pilot dataset Albers NEW"
    )
  )
  expect_equal(
    basename(x$path),
    c("sim_raster_data.yaml",
      "sim_vector_data.yaml",
      "ontario_pilot.yaml",
      "ontario_pilot_albers.yaml",
      "ontario_pilot_albers_NEW.yaml"
    )
  )
  expect_equal(x$status, rep(TRUE, nrow(x)))
})

test_that("multiple user groups", {
  # create object
  d <- system.file("extdata", "projects", package = "wheretowork")
  x <- find_projects(d, c("public", "admin"))
  # arrange tibble name by alphabetical order (needed for github actions)
  x <-  dplyr::arrange(x, name)  
  # tests
  expect_is(x, "tbl_df")
  expect_named(x, c("path", "name", "status"))
  expect_equal(
    x$name,
    c("Example GeoTIFF dataset",
      "Example GeoTIFF dataset (hidden)",
      "Example GeoTIFF dataset (legend manual)",
      "Example GeoTIFF dataset (themes)",
      "Example Shapefile dataset",
      "Example Shapefile dataset (themes)",
      "Ontario pilot dataset",
      "Ontario pilot dataset Albers",
      "Ontario pilot dataset Albers NEW"
    )
  )
  expect_equal(
    basename(x$path),
    c("sim_raster_data.yaml",
      "sim_raster3_data.yaml",
      "sim_raster4_data.yaml",      
      "sim_raster2_data.yaml",
      "sim_vector_data.yaml",
      "sim_vector2_data.yaml",
      "ontario_pilot.yaml",
      "ontario_pilot_albers.yaml",
      "ontario_pilot_albers_NEW.yaml"
    )
  )
  expect_equal(x$status, rep(TRUE, nrow(x)))
})
