context("spatial_data_type")

test_that("spatRaster (continuous)", {
  # prepare data
  rd <- prioritizr::get_sim_features()[[1]]
  non_na <- terra::cells(rd)
  rd[non_na] <- runif(length(non_na))
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "continuous")
})

test_that("spatRaster (categorical)", {
  # prepare data
  rd <- prioritizr::get_sim_features()[[2]]
  non_na <- terra::cells(rd)
  rd[non_na] <- sample(seq_len(5), length(non_na), replace = TRUE)
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "categorical")
})

test_that("vector (continuous)", {
  # prepare data
  rd <- prioritizr::get_sim_pu_polygons()
  rd$var <- runif(nrow(rd))
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "continuous")
})

test_that("vector (categorical)", {
  # prepare data
  rd <- prioritizr::get_sim_pu_polygons()
  rd$var <- sample(seq_len(5), nrow(rd), replace = TRUE)
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "categorical")
})
