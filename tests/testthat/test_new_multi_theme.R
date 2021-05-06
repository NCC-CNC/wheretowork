context("new_multi_theme")

test_that("initialization", {
  # create object
  l1 <- new_layer(source = "l1.txt", total = 100, units = "ha")
  l2 <- new_layer(source = "l2.txt", total = 30, units = "ha")
  f1 <- new_feature(
    name = "F1",
    layer = l1,
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
    layer = l2,
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
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    group_min_goal = 0.01,
    group_max_goal = 0.99,
    group_initial_goal = 0.32,
    group_limit_goal = 0.11,
    group_step_goal = 0.002,
    group_current_label = "Hence")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "MF1")
  expect_identical(x$name, "MF")
  expect_identical(x$feature, list(f1, f2))
  expect_identical(x$initial_status, FALSE)
  expect_identical(x$round, FALSE)
  expect_identical(x$group_min_goal, 0.01)
  expect_identical(x$group_max_goal, 0.99)
  expect_identical(x$group_initial_goal, 0.32)
  expect_identical(x$group_limit_goal, 0.11)
  expect_identical(x$group_step_goal, 0.002)
  expect_identical(x$group_current_label, "Hence")
})

test_that("get methods", {
  # create object
  l1 <- new_layer(source = "l1.txt", total = 100, units = "ha")
  l2 <- new_layer(source = "l2.txt", total = 30, units = "ha")
  f1 <- new_feature(
    name = "F1",
    layer = l1,
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
    layer = l2,
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
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    group_min_goal = 0.01,
    group_max_goal = 0.99,
    group_initial_goal = 0.32,
    group_limit_goal = 0.11,
    group_step_goal = 0.002,
    group_current_label = "Hence")
  # run tests
  expect_identical(x$get_feature_goal(), c(0.2, 0.21))
  expect_identical(x$get_feature_status(), c(FALSE, TRUE))
  expect_identical(x$get_feature_goal(), x$get_parameter("feature_goal"))
  expect_identical(x$get_feature_status(), x$get_parameter("feature_status"))
})

test_that("set methods", {
  # create object
  l1 <- new_layer(source = "l1.txt", total = 100, units = "ha")
  l2 <- new_layer(source = "l2.txt", total = 30, units = "ha")
  f1 <- new_feature(
    name = "F1",
    layer = l1,
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
    layer = l2,
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
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    group_min_goal = 0.01,
    group_max_goal = 0.99,
    group_initial_goal = 0.32,
    group_limit_goal = 0.11,
    group_step_goal = 0.002,
    group_current_label = "Hence")
  # run tests
  x$set_feature_goal(c(0.89, 0.26))
  x$set_feature_status(c(TRUE, FALSE))
  expect_identical(x$get_feature_goal(), c(0.89, 0.26))
  expect_identical(x$get_feature_status(), c(TRUE, FALSE))
  x$set_parameter("feature_goal", c(0.33, 0.67))
  x$set_parameter("feature_status", c(FALSE, FALSE))
  expect_identical(x$get_feature_goal(), c(0.33, 0.67))
  expect_identical(x$get_feature_status(), c(FALSE, FALSE))

})

test_that("widget methods", {
  # create object
  l1 <- new_layer(source = "l1.txt", total = 100, units = "ha")
  l2 <- new_layer(source = "l2.txt", total = 30, units = "ha")
  f1 <- new_feature(
    name = "F1",
    layer = l1,
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
    layer = l2,
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
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    group_min_goal = 0.01,
    group_max_goal = 0.99,
    group_initial_goal = 0.32,
    group_limit_goal = 0.11,
    group_step_goal = 0.002,
    group_current_label = "Hence")
  # run tests
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
        id = "MF1",
        name = "MF",
        feature_name = c("F1", "F2"),
        feature_id = c("FID1", "FID2"),
        feature_total_amount = c(100, 30),
        feature_current_held = c(0.245, 0.523),
        group_min_goal = 0.01,
        group_max_goal = 0.99,
        group_initial_goal = 0.32,
        group_limit_goal = 0.11,
        group_step_goal = 0.002,
        group_current_label = "Hence",
        feature_min_goal = c(0.01, 0.011),
        feature_max_goal = c(0.99, 0.991),
        feature_initial_goal = c(0.2, 0.21),
        feature_limit_goal = c(0.02, 0.021),
        feature_step_goal = c(0.05, 0.051),
        feature_current_label = c("Now", "Here"),
        feature_initial_status = c(FALSE, TRUE),
        feature_icon = c(
          as.character(shiny::icon("bell")), as.character(shiny::icon("adn"))),
        units = "ha",
        initial_status = FALSE,
        round = FALSE,
        icon = as.character(shiny::icon("atom"))
    )
  )
})
