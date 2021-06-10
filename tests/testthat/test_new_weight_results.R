context("new_weight_results")

test_that("initialization", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend())
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  x <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "RID1")
  expect_identical(x$status, FALSE)
  expect_identical(x$factor, 0.2)
  expect_identical(x$held, 0.9)
})

test_that("widget methods", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend())
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  w$factor <- 0.8
  w$status <- TRUE
  x <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID1",
      name = "Human Footprint Index",
      status = TRUE,
      total = v$total,
      factor = 0.8,
      held = 0.9,
      units = v$units,
      type = "weight_results"
    )
  )
})