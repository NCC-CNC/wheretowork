context("new_single_theme")

test_that("initialization", {
  # create object
  l <- new_dataset(
    source = "l1.tif", total = 100, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "F1",
    dataset = l,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.2567,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    mandatory = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "FS1")
  expect_identical(x$name, "FS")
  expect_identical(x$feature, list(f))
  expect_identical(x$mandatory, FALSE)
  expect_identical(x$round, FALSE)
  expect_identical(x$icon, shiny::icon("atom"))
})

test_that("get methods", {
  # create object
  l <- new_dataset(
    source = "l1.tif", total = 100, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "F1",
    dataset = l,
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.2567,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_identical(x$get_feature_goal(), 0.2)
  expect_identical(x$get_feature_visible(), TRUE)
  expect_identical(x$get_feature_status(), FALSE)
  expect_identical(x$get_parameter("feature_goal"), x$get_feature_goal())
  expect_identical(x$get_parameter("feature_status"), x$get_feature_status())
  expect_identical(x$get_parameter("feature_visible"), x$get_feature_visible())
})

test_that("set methods", {
  # create object
  l <- new_dataset(
    source = "l1.tif", total = 100, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "F1",
    dataset = l,
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.2567,
    current_label = "Now",
    icon = "bell",
    id = "FID1")
  x <- new_single_theme(
    name = "FS",
    feature = f,
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  x$set_feature_goal(0.8)
  x$set_feature_visible(FALSE)
  x$set_feature_status(TRUE)
  expect_identical(x$get_feature_goal(), 0.8)
  expect_identical(x$get_feature_visible(), FALSE)
  expect_identical(x$get_feature_status(), TRUE)
  x$set_parameter("feature_goal", 0.5)
  x$set_parameter("feature_status", FALSE)
  x$set_parameter("feature_visible", TRUE)
  expect_identical(x$get_feature_goal(), 0.5)
  expect_identical(x$get_feature_status(), FALSE)
  expect_identical(x$get_feature_visible(), TRUE)
})

test_that("widget methods", {
  # create object
  l <- new_dataset(
    source = "l1.tif", total = 100, units = "ha",
    legend = simulate_continuous_legend())
  f <- new_feature(
    name = "F1",
    dataset = l,
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
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FS1",
      name = "FS",
      feature_name = "F1",
      feature_id = "FID1",
      feature_status = FALSE,
      feature_total_amount = 100,
      feature_current_held = 0.034,
      feature_min_goal = 0.01,
      feature_max_goal = 0.99,
      feature_goal = 0.2,
      feature_limit_goal = 0.05,
      feature_step_goal = 0.02,
      feature_current_label = "Now",
      units = "ha",
      mandatory = TRUE,
      round = FALSE,
      icon = as.character(shiny::icon("atom")))
  )
  ## map manager
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FS1",
      name = "FS",
      feature_name = "F1",
      feature_id = "FID1",
      feature_visible = FALSE,
      feature_legend = l$legend$get_widget_data(),
      units = "ha",
      type = "theme")
  )
})
