context("new_theme")

test_that("SingleTheme", {
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
  y <- new_theme(
    name = "FS",
    feature = f,
    initial_status = FALSE,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  # run tests
  expect_equal(x, y)
})

test_that("MultiTheme", {
  # create object
  l1 <- new_layer(source = "l1.txt", current = 0.2, total = 100, units = "ha")
  l2 <- new_layer(source = "l2.txt", current = 0.5, total = 30, units = "ha")
  f1 <- new_feature(
    name = "F1",
    layer = l1,
    initial_status = FALSE,
    initial_goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
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
  y <- new_theme(
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
  expect_equal(x, y)

})
