context("new_variable")

test_that("initialization", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  d <- new_dataset(rd)
  l <- new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
  # create object
  x <- new_variable(
    dataset = d, index = 2, total = 12, units = "ha", legend = l)
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, 2)
  expect_identical(x$total, 12)
  expect_identical(x$units, "ha")
  expect_identical(x$legend, l)
})

test_that("methods", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  d <- new_dataset(rd)
  l <- new_continuous_legend(1, 100, c("#000000", "#AAAAAA"))
  # create object
  x <- new_variable(
    dataset = d, index = 2, total = 12, units = "ha", legend = l)
  # run tests
  expect_true(x$verify())
  x$index <- 1000
  expect_error(x$verify())
  x$index <- 2
  expect_equal(x$get_data(), x$dataset$get_index(2))
})

test_that("new_variable_from_auto (continuous)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  d <- new_dataset(rd)
  # create object
  x <- new_variable_from_auto(
    dataset = d, index = 2, units = "ha", colors = "viridis")
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, 2)
  expect_identical(x$total, raster::cellStats(rd[[2]], "sum"))
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
   new_continuous_legend(
     raster::cellStats(rd[[2]], "min"),
     raster::cellStats(rd[[2]], "max"),
     color_palette("viridis", 20)))
})

test_that("new_variable_from_auto (categorical)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  non_na <- raster::Which(!is.na(rd[[2]]), cells = TRUE)
  rd[[2]][non_na] <-
    as.double(sample(seq_len(5), length(non_na), replace = TRUE))
  d <- new_dataset(rd)
  # create object
  x <- new_variable_from_auto(
    dataset = d, index = 2, units = "ha", colors = "viridis")
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$total, raster::cellStats(rd[[2]], "sum"))
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_categorical_legend(seq(1, 5), color_palette("viridis", 5))
  )
})

test_that("new_variable_from_metadata (continuous)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  d <- new_dataset(rd)
  # create object
  x <- new_variable_from_metadata(
    dataset = d,
    metadata =
      list(
        index = 2, type = "continuous", units = "ha", colors = "viridis",
        min_value = 1, max_value = 5, total = 11))
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, 2)
  expect_identical(x$total, 11)
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_continuous_legend(1, 5, color_palette("viridis", 20))
  )
})

test_that("new_variable_from_metadata (categorical)", {
  # prepare data
  data(sim_features, package = "prioritizr")
  rd <- sim_features
  d <- new_dataset(rd)
  # create object
  x <- new_variable_from_metadata(
    dataset = d,
    metadata =
      list(
        index = 2, type = "categorical", units = "ha", colors = "viridis",
        total = 11, values = seq(1, 6)))
  # run tests
  expect_is(x, "Variable")
  expect_is(x$repr(), "character")
  expect_identical(x$dataset, d)
  expect_identical(x$index, 2)
  expect_identical(x$total, 11)
  expect_identical(x$units, "ha")
  expect_equal(
    x$legend,
    new_categorical_legend(seq(1, 6), color_palette("viridis", 6))
  )
})
