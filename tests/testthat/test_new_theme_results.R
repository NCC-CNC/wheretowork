context("new_theme_results")

test_that("initialization (SingleTheme)", {
  # create objects
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID2")
  # run tests
  expect_identical(x$id, "RID2")
  expect_identical(x$theme, th)
  expect_identical(x$feature_results[[1]], fr)
})

test_that("results methods (SingleTheme)", {
  # create objects
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  v <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend()
  )
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  f$status <- TRUE
  f$goal <- 0.97
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID2")
  # run tests
  expect_identical(
    x$get_results_data(),
    tibble::tibble(
      name = th$name,
      feature_name = f$name,
      feature_status = fr$status,
      feature_total_amount = v$total,
      feature_current_held = f$current,
      feature_goal = fr$goal,
      feature_solution_held = fr$held,
      units = v$units,
    )
  )
})

test_that("widget methods (SingleTheme)", {
  # create objects
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  v <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend()
  )
  f <- new_feature(
    name = "F1",
    variable = v,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    step_goal = 0.02,
    limit_goal = 0.05,
    current = 0.034,
    id = "FID1"
  )
  f$status <- TRUE
  f$goal <- 0.97
  fr <- new_feature_results(
    feature = f,
    held = 0.9,
    id = "RID1"
  )
  th <- new_theme(
    name = "FS",
    feature = f,
    id = "FS1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID2")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID2",
      name = th$name,
      feature_name = f$name,
      feature_id = fr$id,
      feature_status = fr$status,
      feature_total_amount = v$total,
      feature_current_held = f$current,
      feature_goal = fr$goal,
      feature_solution_held = fr$held,
      units = v$units,
      type = "theme_results"
    )
  )
})

test_that("initialization (MultiTheme)", {
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 30, units = "ha",
    legend = simulate_continuous_legend()
  )
  f1 <- new_feature(
    name = "F1",
    variable = v1,
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = v2,
    visible = FALSE,
    status = TRUE,
    goal = 0.21,
    min_goal = 0.011,
    max_goal = 0.991,
    limit_goal = 0.021,
    step_goal = 0.051,
    current = 0.5,
    id = "FID2"
  )
  fr <- list(
    new_feature_results(
      feature = f1,
      held = 0.89,
      id = "RID1"
    ),
    new_feature_results(
      feature = f2,
      held = 0.89,
      id = "RID2"
    )
  )
  th <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID3")
  # run tests
  expect_identical(x$id, "RID3")
  expect_identical(x$theme, th)
  expect_identical(x$feature_results, fr)
})

test_that("results methods (MultiTheme)", {
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 30, units = "ha",
    legend = simulate_continuous_legend()
  )
  f1 <- new_feature(
    name = "F1",
    variable = v1,
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = v2,
    visible = FALSE,
    status = TRUE,
    goal = 0.21,
    min_goal = 0.011,
    max_goal = 0.991,
    limit_goal = 0.021,
    step_goal = 0.051,
    current = 0.5,
    id = "FID2"
  )
  fr <- list(
    new_feature_results(
      feature = f1,
      held = 0.89,
      id = "RID1"
    ),
    new_feature_results(
      feature = f2,
      held = 0.91,
      id = "RID2"
    )
  )
  th <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID3")
  # run tests
  expect_identical(
    x$get_results_data(),
    tibble::tibble(
      name = th$name,
      feature_name = c(fr[[1]]$feature$name, fr[[2]]$feature$name),
      feature_status = c(fr[[1]]$feature$status, fr[[2]]$feature$status),
      feature_total_amount = c(
        fr[[1]]$feature$variable$total, fr[[2]]$feature$variable$total
      ),
      feature_current_held = c(
        fr[[1]]$feature$current, fr[[2]]$feature$current
      ),
      feature_goal = c(fr[[1]]$goal, fr[[2]]$goal),
      feature_solution_held = c(fr[[1]]$held, fr[[2]]$held),
      units = v1$units,
    )
  )
})

test_that("widget methods (MultiTheme)", {
  # create object
  rd <- simulate_proportion_spatial_data(import_simple_raster_data(), 2)
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    dataset = d, index = 1, total = 100, units = "ha",
    legend = simulate_continuous_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 30, units = "ha",
    legend = simulate_continuous_legend()
  )
  f1 <- new_feature(
    name = "F1",
    variable = v1,
    visible = TRUE,
    status = FALSE,
    goal = 0.2,
    min_goal = 0.01,
    max_goal = 0.99,
    limit_goal = 0.02,
    step_goal = 0.05,
    current = 0.245,
    id = "FID1"
  )
  f2 <- new_feature(
    name = "F2",
    variable = v2,
    visible = FALSE,
    status = TRUE,
    goal = 0.21,
    min_goal = 0.011,
    max_goal = 0.991,
    limit_goal = 0.021,
    step_goal = 0.051,
    current = 0.5,
    id = "FID2"
  )
  fr <- list(
    new_feature_results(
      feature = f1,
      held = 0.89,
      id = "RID1"
    ),
    new_feature_results(
      feature = f2,
      held = 0.91,
      id = "RID2"
    )
  )
  th <- new_multi_theme(
    name = "MF",
    feature = list(f1, f2),
    id = "MF1"
  )
  x <- new_theme_results(theme = th, feature_results = fr, id = "RID3")
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID3",
      name = th$name,
      feature_name = c(fr[[1]]$feature$name, fr[[2]]$feature$name),
      feature_id = c(fr[[1]]$id, fr[[2]]$id),
      feature_status = c(fr[[1]]$feature$status, fr[[2]]$feature$status),
      feature_total_amount = c(
        fr[[1]]$feature$variable$total, fr[[2]]$feature$variable$total
      ),
      feature_current_held = c(
        fr[[1]]$feature$current, fr[[2]]$feature$current
      ),
      feature_goal = c(fr[[1]]$goal, fr[[2]]$goal),
      feature_solution_held = c(fr[[1]]$held, fr[[2]]$held),
      units = v1$units,
      type = "theme_results"
    )
  )
})
