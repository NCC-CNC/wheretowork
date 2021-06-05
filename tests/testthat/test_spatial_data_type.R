context("spatial_data_type")

test_that("raster (continuous)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- raster::Which(!is.na(rd), cells = TRUE)
  rd[non_na] <- runif(length(non_na))
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "continuous")
})

test_that("raster (categorical)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- raster::Which(!is.na(rd), cells = TRUE)
  rd[non_na] <- sample(seq_len(5), length(non_na), replace = TRUE)
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "categorical")
})

test_that("vector (continuous)", {
  # prepare data
  data(sim_pu_sf, package = "prioritizr")
  rd <- sim_pu_sf
  rd$var <- runif(nrow(rd))
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "continuous")
})

test_that("vector (categorical)", {
  # prepare data
  data(sim_pu_sf, package = "prioritizr")
  rd <- sim_pu_sf
  rd$var <- sample(seq_len(5), nrow(rd), replace = TRUE)
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_type(rd)
  # run tests
  expect_identical(x, "categorical")
})
