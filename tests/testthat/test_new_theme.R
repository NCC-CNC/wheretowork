context("new_theme")

test_that("SingleTheme", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "F1",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.034,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  y <- new_theme(
    name = "FS",
    feature = f,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_equal(x, y)
})

test_that("MultiTheme", {
  # create object
  f <- system.file("extdata", "sim_raster_data.tif", package = "locationmisc")
  d <- new_dataset(f)
  v1 <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend())
  v2 <- new_variable(
    dataset = d, index = 2, total = 30, units = "ha",
    legend = simulate_continuous_legend())
  f1 <- new_feature(
    name = "F1",
    variable = v1,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.2,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  f2 <- new_feature(
    name = "F2",
    variable = v2,
    initial_visible = FALSE,
    initial_status = TRUE,
    initial_goal = 0.21,
    min_goal = 0.011,
    max_goal = 0.991,
    limit_goal = 0.021,
    step_goal = 0.051,
    current = 0.5,
    current_label = "Here",
    icon = "adn",
    id = "FID2")
  x <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  y <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  # run tests
  expect_equal(x, y)
})
