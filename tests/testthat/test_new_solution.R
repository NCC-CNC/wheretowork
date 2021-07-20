context("new_solution")

test_that("initialization", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1"
  )
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$visible, FALSE)
  expect_identical(x$visible, FALSE)
  expect_identical(x$statistics, list(s1, s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list(wr))
  expect_identical(x$id, "solution1")
  expect_is(x$get_theme_results_data(), "data.frame")
  expect_is(x$get_weight_results_data(), "data.frame")
  expect_is(x$render_theme_results(), "datatables")
  expect_is(x$render_weight_results(), "datatables")
})

test_that("get methods", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), FALSE)
})

test_that("set methods", {
  # create object
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), FALSE)
  x$set_visible(TRUE)
  expect_equal(x$get_visible(), TRUE)
})

test_that("widget methods", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")
    )
  )
  v2 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v3 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1"
  )
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2"
  )
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1"
  )
  # run tests
  ## solution results widget
  expect_identical(
    x$get_solution_results_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      statistics = list(s1$get_widget_data(), s2$get_widget_data()),
      theme_results = list(thr$get_widget_data()),
      weight_results = list(wr$get_widget_data()),
      solution_color = scales::alpha(last(x$variable$legend$colors), 1)
    )
  )
  ## map manager widget
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      visible = FALSE,
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution"
    )
  )
})
