context("spatial_data_statistics")

test_that("raster (continuous)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- terra::cells(rd)
  rd[non_na] <- runif(length(non_na))
  # run function
  x <- spatial_data_statistics(rd, "continuous")
  # run tests
  expect_identical(
    x,
    list(
      total = terra::global(rd, fun="sum", na.rm=TRUE)[[1]],
      min_value = terra::global(rd, fun="min", na.rm=TRUE)[[1]],
      max_value = terra::global(rd, fun="max", na.rm=TRUE)[[1]]
    )
  )
})

test_that("raster (categorical)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features[[2]]
  non_na <- terra::cells(rd)
  rd[non_na] <- as.double(sample(seq_len(5), length(non_na), replace = TRUE))
  # run function
  x <- spatial_data_statistics(rd, "categorical")
  # run tests
  expect_identical(
    x,
    list(
      total = terra::global(rd, fun="sum", na.rm=TRUE)[[1]],
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
