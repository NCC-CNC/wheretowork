context("new_feature_results")

test_that("initialization", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "Intact Alvar",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current = 0.56,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "RID1")
  expect_identical(x$status, FALSE)
  expect_identical(x$goal, 0.2)
  expect_identical(x$held, 0.9)
})

test_that("widget methods", {
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "Intact Alvar",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.9,
    step_goal = 0.03,
    limit_goal = 0.2,
    current = 0.56,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  f$goal <- 0.5
  x <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID1",
      name = "Intact Alvar",
      status = FALSE,
      goal = 0.5,
      held = 0.9,
      legend = v$legend$get_widget_data(),
      units = v$units
    )
  )
})
