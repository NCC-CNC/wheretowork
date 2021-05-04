context("new_single_theme")

test_that("initialization", {
  # create object
  l <- new_layer(source = "l1.tif", current = 0.2, total = 100, units = "ha")
  f <- new_feature(
    name = "F1",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_identical(x$id, "FS1")
  expect_identical(x$name, "FS")
  expect_identical(x$feature, list(f))
  expect_identical(x$initial_status, FALSE)
  expect_identical(x$round, FALSE)
  expect_identical(x$icon, shiny::icon("atom"))
})

test_that("get methods", {
  # create object
  l <- new_layer(source = "l1.tif", current = 0.2, total = 100, units = "ha")
  f <- new_feature(
    name = "F1",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_identical(x$get_feature_goal(), 0.2)
  expect_identical(x$get_feature_status(), FALSE)
  expect_identical(x$get_parameter("feature_goal"), x$get_feature_goal())
  expect_identical(x$get_parameter("feature_status"), x$get_feature_status())
})

test_that("set methods", {
  # create object
  l <- new_layer(source = "l1.tif", current = 0.2, total = 100, units = "ha")
  f <- new_feature(
    name = "F1",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  x$set_feature_goal(0.8)
  x$set_feature_status(TRUE)
  expect_identical(x$get_feature_goal(), 0.8)
  expect_identical(x$get_feature_status(), TRUE)
  x$set_parameter("feature_goal", 0.5)
  x$set_parameter("feature_status", FALSE)
  expect_identical(x$get_feature_goal(), 0.5)
  expect_identical(x$get_feature_status(), FALSE)
})

test_that("widget methods", {
  # create object
  l <- new_layer(source = "l1.tif", current = 0.034, total = 100, units = "ha")
  f <- new_feature(
    name = "F1",
    layer = l,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FS1",
      name = "FS",
      feature_name = "F1",
      feature_id = "FID1",
      feature_total_amount = 100,
      feature_current_held = 0.034,
      feature_min_goal = 0.01,
      feature_max_goal = 0.99,
      feature_initial_goal = 0.2,
      feature_limit_goal = 0.05,
      feature_step_goal = 0.02,
      feature_current_label = "Now",
      units = "ha",
      initial_status = FALSE,
      round = FALSE,
      icon = as.character(shiny::icon("atom")))
  )
})
