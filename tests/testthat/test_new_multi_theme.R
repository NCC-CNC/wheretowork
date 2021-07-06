context("new_multi_theme")

test_that("initialization", {
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
    mandatory = FALSE,
    id = "MF1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "MF1")
  expect_identical(x$name, "MF")
  expect_identical(x$feature, list(f1, f2))
  expect_identical(x$mandatory, FALSE)
})

test_that("get methods", {
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
    mandatory = FALSE,
    id = "MF1")
  # run tests
  expect_identical(x$get_feature_goal(), c(0.2, 0.21))
  expect_identical(x$get_feature_visible(), c(TRUE, FALSE))
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_feature_status(), c(FALSE, TRUE))
  expect_identical(x$get_feature_current(), c(0.245, 0.5))
  expect_identical(x$get_feature_goal(), x$get_setting("feature_goal"))
  expect_identical(x$get_feature_status(), x$get_setting("feature_status"))
  expect_identical(x$get_feature_visible(), x$get_setting("feature_visible"))
  expect_identical(x$get_feature_current(), x$get_setting("feature_current"))
  expect_identical(x$get_setting("visible"), x$get_visible())
})

test_that("set methods", {
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
    mandatory = FALSE,
    id = "MF1")
  # run tests
  x$set_feature_goal(c(0.89, 0.26))
  x$set_feature_status(c(TRUE, FALSE))
  x$set_feature_visible(c(FALSE, FALSE))
  x$set_feature_current(c(0.43, 0.21))
  expect_identical(x$get_feature_goal(), c(0.89, 0.26))
  expect_identical(x$get_feature_status(), c(TRUE, FALSE))
  expect_identical(x$get_feature_visible(), c(FALSE, FALSE))
  expect_identical(x$get_feature_current(), c(0.43, 0.21))
  x$set_setting("feature_goal", c(0.33, 0.67))
  x$set_setting("feature_status", c(FALSE, FALSE))
  x$set_setting("feature_visible", c(TRUE, TRUE))
  x$set_setting("feature_current", c(0.22, 0.99))
  expect_identical(x$get_feature_goal(), c(0.33, 0.67))
  expect_identical(x$get_feature_status(), c(FALSE, FALSE))
  expect_identical(x$get_feature_visible(), c(TRUE, TRUE))
  expect_identical(x$get_feature_current(), c(0.22, 0.99))
  x$set_visible(FALSE)
  expect_identical(x$get_visible(), FALSE)
})

test_that("export method", {
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
    mandatory = FALSE,
    id = "MF1")
  # run tests
  expect_equal(
    x$export(),
    list(
      name = x$name,
      mandatory = x$mandatory,
      feature = lapply(x$feature, function(x) x$export())
    )
  )
})

test_that("widget methods", {
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
    current = 0.523,
    id = "FID2")
  x <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    mandatory = FALSE,
    id = "MF1")
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
        id = "MF1",
        name = "MF",
        feature_name = c("F1", "F2"),
        feature_id = c("FID1", "FID2"),
        feature_status = c(FALSE, TRUE),
        feature_total_amount = c(f1$variable$total, f2$variable$total),
        feature_current_held = c(0.245, 0.523),
        feature_min_goal = c(0.01, 0.011),
        feature_max_goal = c(0.99, 0.991),
        feature_goal = c(0.2, 0.21),
        feature_limit_goal = c(0.02, 0.021),
        feature_step_goal = c(0.05, 0.051),
        units = "ha",
        mandatory = FALSE)
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
        id = "MF1",
        name = "MF",
        feature_name = c("F1", "F2"),
        feature_id = c("FID1", "FID2"),
        feature_visible = c(TRUE, FALSE),
        feature_legend =
          lapply(x$feature, function(x) x$variable$legend$get_widget_data()),
          units = "ha",
        type = "theme")
  )
})
