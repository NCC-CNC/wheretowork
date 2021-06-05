context("new_multi_theme")

test_that("initialization", {
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
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
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
    mandatory = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "MF1")
  expect_identical(x$name, "MF")
  expect_identical(x$feature, list(f1, f2))
  expect_identical(x$mandatory, FALSE)
  expect_identical(x$round, FALSE)
  expect_identical(x$current_label, "Hence")
})

test_that("get methods", {
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
    initial_visible = TRUE,
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
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  # run tests
  expect_identical(x$get_feature_goal(), c(0.2, 0.21))
  expect_identical(x$get_feature_visible(), c(TRUE, FALSE))
  expect_identical(x$get_feature_status(), c(FALSE, TRUE))
  expect_identical(x$get_feature_goal(), x$get_parameter("feature_goal"))
  expect_identical(x$get_feature_status(), x$get_parameter("feature_status"))
  expect_identical(x$get_feature_visible(), x$get_parameter("feature_visible"))
})

test_that("set methods", {
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
    initial_visible = TRUE,
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
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  # run tests
  x$set_feature_goal(c(0.89, 0.26))
  x$set_feature_status(c(TRUE, FALSE))
  x$set_feature_visible(c(FALSE, FALSE))
  expect_identical(x$get_feature_goal(), c(0.89, 0.26))
  expect_identical(x$get_feature_status(), c(TRUE, FALSE))
  expect_identical(x$get_feature_visible(), c(FALSE, FALSE))
  x$set_parameter("feature_goal", c(0.33, 0.67))
  x$set_parameter("feature_status", c(FALSE, FALSE))
  x$set_parameter("feature_visible", c(TRUE, TRUE))
  expect_identical(x$get_feature_goal(), c(0.33, 0.67))
  expect_identical(x$get_feature_status(), c(FALSE, FALSE))
  expect_identical(x$get_feature_visible(), c(TRUE, TRUE))
})

test_that("widget methods", {
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
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
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
    current = 0.523,
    current_label = "Here",
    icon = "adn",
    id = "FID2")
  x <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    mandatory = TRUE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
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
        feature_total_amount = c(100, 30),
        feature_current_held = c(0.245, 0.523),
        feature_min_goal = c(0.01, 0.011),
        feature_max_goal = c(0.99, 0.991),
        feature_goal = c(0.2, 0.21),
        feature_limit_goal = c(0.02, 0.021),
        feature_step_goal = c(0.05, 0.051),
        feature_current_label = c("Now", "Here"),
        feature_icon = c(
          as.character(shiny::icon("bell")), as.character(shiny::icon("adn"))),
        units = "ha",
        mandatory = TRUE,
        current_label = "Hence",
        round = FALSE,
        icon = as.character(shiny::icon("atom")))
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
