context("new_theme")

test_that("SingleTheme", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
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
    current = 0.2567,
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    id = "FS1")
  y <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1")
  # run tests
  expect_equal(x, y)
})

test_that("MultiTheme", {
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  f1 <- new_feature(
    name = "F1",
    variable = new_variable_from_auto(dataset = d, index = 1, units = "ha"),
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
    id = "FID1")
  f2 <- new_feature(
    name = "F2",
    variable = new_variable_from_auto(dataset = d, index = 2, units = "ha"),
    initial_visible = FALSE,
    initial_status = TRUE,
    initial_goal = 0.21,
    min_goal = 0.011,
    max_goal = 0.991,
    limit_goal = 0.021,
    step_goal = 0.051,
    current = 0.5,
    id = "FID2")
  x <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1")
  y <- new_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1")
  # run tests
  expect_equal(x, y)
})
