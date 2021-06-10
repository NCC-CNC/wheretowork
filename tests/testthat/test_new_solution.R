context("new_solution")

test_that("initialization", {
  # create object
  d <- new_dataset(tempfile())
  v1 <- new_variable(
    d, index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")))
  v2 <- new_variable(
    d, index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")))
  v3 <- new_variable(
    d, index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")))
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1")
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2")
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    primary_statistics = list(s1),
    supplementary_statistics = list(s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1")
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$primary_statistics, list(s1))
  expect_identical(x$supplementary_statistics, list(s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list(wr))
  expect_identical(x$id, "solution1")
})

test_that("widget methods", {
  # create object
  d <- new_dataset(tempfile())
  v1 <- new_variable(
    d, index = 1, total = 45, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 100, colors = c("#FFFFFF", "#112233")))
  v2 <- new_variable(
    d, index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")))
  v3 <- new_variable(
    d, index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")))
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  wr <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1")
  f <- new_feature(
    name = "F1",
    variable = v2,
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
  thr <- new_theme_results(
    theme = th,
    feature_results = fr,
    id = "RID2")
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    primary_statistics = list(s1),
    supplementary_statistics = list(s2),
    theme_results = list(thr),
    weight_results = list(wr),
    id = "solution1")
  # run tests
  ## solution results widget
  expect_identical(
    x$get_solution_results_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      statistics = list(s1$get_widget_data(), s2$get_widget_data()),
      theme_results = list(thr$get_widget_data()),
      weight_results = list(wr$get_widget_data())
    )
  )
  ## map manager widget
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      statistics = list(s1$get_widget_data()),
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution"
    )
  )
})

# test_that("new_solution_from_prioritization", {
#   # create objects
#   ## raw data
#   f <- system.file("extdata", "sim_vector_data.gpkg", package = "locationmisc")
#   rd <- sf::read_sf(f)
#   rd <- rd[seq_len(min(100, nrow(rd))), drop = FALSE]
#   ## set names
#   theme_names <-
#     list(theme1 = names(rd)[1],
#          theme2 = names(rd)[2],
#          theme3 = names(rd)[3:8])
#   wt_names <- names(rd)[11:15]
#   ## create dataset
#   d <- new_dataset(rd)
#   ## create weights
#   wts <- lapply(wt_names, function(x) {
#     v <- new_variable_from_auto(dataset = d, index = x)
#     new_weight(x, variable = v, initial_factor = runif(1, 0, 100))
#   })
#   ## create themes
#   thms <- lapply(seq_along(theme_names), function(i) {
#     fts <- lapply(theme_names[[i]], function(x) {
#       v <- new_variable_from_auto(dataset = d, index = x)
#       new_feature(x, variable = v, initial_goal = runif(1, 0.2, 1))
#     })
#     new_theme(name = names(theme_names)[[i]], feature = fts)
#   })
#   ## create solution settings object
#   ss <- new_solution_settings(themes = thms, weights = wts)
#   ## create matrices
#   rij <- ss$get_rij_matrix()
#   wij <- ss$get_wij_matrix()
#   ## create object
#   x <- new_solution_from_prioritization(
#     name = "solution01", dataset = d,
#     solution_settings = ss,
#     rij = rij, wij = wij, boundary_data = prioritizr::boundary_matrix(rd),
#     gap = 0.001, boundary_penalty_gap = 0.1)
#   # run tests
#   # TODO
#
# })
