context("spatial_data_statistics")

test_that("spatRaster (continuous)", {
  # prepare data
  rd <- prioritizr::get_sim_features()[[1]]
  non_na <- terra::cells(rd)
  rd[non_na] <- runif(length(non_na))
  # run function
  x <- spatial_data_statistics(rd, "continuous")
  # run tests
  expect_identical(
    x,
    list(
      total = terra::global(rd, fun="sum", na.rm=TRUE)$sum,
      min_value = terra::global(rd, fun="min", na.rm=TRUE)$min,
      max_value = terra::global(rd, fun="max", na.rm=TRUE)$max
    )
  )
})

test_that("spatRaster (categorical)", {
  # prepare data
  rd <- prioritizr::get_sim_features()[[2]]
  non_na <- terra::cells(rd)
  rd[non_na] <- as.double(sample(seq_len(5), length(non_na), replace = TRUE))
  # run function
  x <- spatial_data_statistics(rd, "categorical")
  # run tests
  expect_identical(
    x,
    list(
      total = terra::global(rd, fun="sum", na.rm=TRUE)$sum,
      values = as.double(seq_len(5))
    )
  )
})

test_that("vector (continuous)", {
  # prepare data
  rd <- prioritizr::get_sim_pu_polygons()
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
  rd <- prioritizr::get_sim_pu_polygons()
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
