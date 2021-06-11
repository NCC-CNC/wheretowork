context("new_theme_results")

test_that("initialization (SingleTheme)", {
  # create objects
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
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1")
  th <- new_theme(
    name = "FS",
    feature = f,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID2")
  # run tests
  expect_identical(x$id, "RID2")
  expect_identical(x$theme, th)
  expect_identical(x$feature_results[[1]], fr)
})

test_that("widget methods (SingleTheme)", {
  # create objects
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
  f$status <- TRUE
  f$goal <- 0.97
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1")
  th <- new_theme(
    name = "FS",
    feature = f,
    round = FALSE,
    icon = "atom",
    id = "FS1")
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID2")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID2",
      name = th$name,
      feature_name = f$name,
      feature_id = f$id,
      feature_status = fr$status,
      feature_total_amount = v$total,
      feature_current_held = f$current,
      feature_goal = fr$goal,
      feature_solution_held = fr$held,
      units = v$units,
      mandatory = th$mandatory,
      round = th$round,
      icon = as.character(th$icon),
      type ="theme_results"
    )
  )
})

test_that("initialization (MultiTheme)", {
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
  fr <- list(
    new_feature_results(
      feature = f1,
      held = 0.89,
      id = "RID1"),
    new_feature_results(
      feature = f2,
      held = 0.89,
      id = "RID2"))
  th <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    mandatory = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID3")
  # run tests
  expect_identical(x$id, "RID3")
  expect_identical(x$theme, th)
  expect_identical(x$feature_results, fr)
})

test_that("widget methods (MultiTheme)", {
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
  fr <- list(
    new_feature_results(
      feature = f1,
      held = 0.89,
      id = "RID1"),
    new_feature_results(
      feature = f2,
      held = 0.91,
      id = "RID2"))
  th <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    mandatory = FALSE,
    round = FALSE,
    icon = "atom",
    id = "MF1",
    current_label = "Hence")
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID3")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID3",
      name = th$name,
      feature_name = c(fr[[1]]$feature$name, fr[[2]]$feature$name),
      feature_id = c(fr[[1]]$feature$id, fr[[2]]$feature$id),
      feature_status = c(fr[[1]]$feature$status, fr[[2]]$feature$status),
      feature_total_amount = c(
        fr[[1]]$feature$variable$total, fr[[2]]$feature$variable$total),
      feature_current_held = c(
        fr[[1]]$feature$current, fr[[2]]$feature$current),
      feature_goal = c(fr[[1]]$goal, fr[[2]]$goal),
      feature_solution_held = c(fr[[1]]$held, fr[[2]]$held),
      units = v1$units,
      mandatory = th$mandatory,
      round = th$round,
      icon = as.character(th$icon),
      type ="theme_results"
    )
  )
})