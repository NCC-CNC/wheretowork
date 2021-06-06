context("spatial_data_statistics")

test_that("raster (continuous)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- raster::Which(!is.na(rd), cells = TRUE)
  rd[non_na] <- runif(length(non_na))
  # run function
  x <- spatial_data_statistics(rd, "continuous")
  # run tests
  expect_identical(
    x,
    list(
      total = raster::cellStats(rd, "sum"),
      min_value = raster::cellStats(rd, "min"),
      max_value = raster::cellStats(rd, "max")
    )
  )
})

test_that("raster (categorical)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- raster::Which(!is.na(rd), cells = TRUE)
  rd[non_na] <- as.double(sample(seq_len(5), length(non_na), replace = TRUE))
  # run function
  x <- spatial_data_statistics(rd, "categorical")
  # run tests
  expect_identical(
    x,
    list(
      total = raster::cellStats(rd, "sum"),
      values = as.double(seq_len(5))
    )
  )
})

test_that("vector (continuous)", {
  # prepare data
  data(sim_pu_sf, package = "prioritizr")
  rd <- sim_pu_sf
  rd$var <- runif(nrow(rd))
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_statistics(rd, "continuous")
  # run tests
  expect_identical(
    x,
    list(
      total = sum(rd[[1]]),
      min_value = min(rd[[1]]),
      max_value = max(rd[[1]])
    )
  )
})

test_that("vector (categorical)", {
  # prepare data
  data(sim_pu_sf, package = "prioritizr")
  rd <- sim_pu_sf
  rd$var <- as.double(sample(seq_len(5), nrow(rd), replace = TRUE))
  rd <- rd[, "var"]
  # run function
  x <- spatial_data_statistics(rd, "categorical")
  # run tests
  expect_identical(
    x,
    list(
      total = sum(rd[[1]]),
      values = as.double(seq_len(5))
    )
  )
})
