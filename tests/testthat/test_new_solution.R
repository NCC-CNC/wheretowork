context("new_solution")

test_that("initialization", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  names(rd) <- c("a", "b", "solution_1", "c", "d")
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
  v4 <- new_variable(
    d,
    index = 3, total = 90, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  v5 <- new_variable(
    d,
    index = 4, total = 20, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )  
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = -0.2,
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
  i <- new_include(
    name = "Protected areas",
    variable = v4,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  ir <- new_include_results(
    include = i,
    held = 0.78,
    id = "IID1"
  )
  excl <- new_exclude(
    name = "Highways",
    variable = v5,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  exclr <- new_exclude_results(
    exclude = excl,
    held = 0.78,
    id = "EID1"
  )  
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  p1 <- new_parameter("Budget", value = 12, status = FALSE, units = "%")
  p2 <- new_parameter("Cluster", value = 50, units = "%")
  p3 <- new_parameter("Hide")
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p1, p2, p3),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    exclude_results = list(exclr),
    id = "solution1",
    downloadable = TRUE
  )
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$visible, FALSE)
  expect_identical(x$invisible, NA_real_)
  expect_identical(x$loaded, FALSE)
  expect_equal(x$parameters, list(p1, p2, p3))
  expect_identical(x$statistics, list(s1, s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list(wr))
  expect_identical(x$include_results, list(ir))
  expect_identical(x$exclude_results, list(exclr))
  expect_identical(x$id, "solution1")
  expect_is(x$get_summary_results_data(), "data.frame")
  expect_is(x$get_theme_results_data(), "data.frame")
  expect_is(x$get_weight_results_data(), "data.frame")
  expect_is(x$get_include_results_data(), "data.frame")
  expect_is(x$render_summary_results(), "datatables")
  expect_is(x$render_theme_results(), "datatables")
  expect_is(x$render_weight_results(), "datatables")
  expect_is(x$render_include_results(), "datatables")
  expect_identical(x$downloadable, TRUE)
  
  ## summary results tibble (parameter name/values)
  expect_identical(
    unlist(x$get_summary_results_data()[1,], use.names = FALSE), 
    c("Budget", "Not specified")
  )
  expect_identical(
    unlist(x$get_summary_results_data()[2,], use.names = FALSE), 
    c("Cluster", "50%")
  )
  expect_identical(
    unlist(x$get_summary_results_data()[3,], use.names = FALSE), 
    c("Hide", "On")
  ) 
})

test_that("initialization (no weights, includes or excludes)", {
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
  f <- new_feature(
    name = "F1",
    variable = v2,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
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
  p <- new_parameter("budget", value = 12)
  x <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    exclude_results = list(),
    id = "solution1",
    downloadable = TRUE
  )
  # run tests
  expect_is(x, "Solution")
  print(x)
  expect_identical(x$name, "solution001")
  expect_identical(x$variable, v3)
  expect_identical(x$visible, FALSE)
  expect_identical(x$invisible, NA_real_)
  expect_identical(x$loaded, FALSE)
  expect_equal(x$parameters, list(p))
  expect_identical(x$statistics, list(s1, s2))
  expect_identical(x$theme_results, list(thr))
  expect_identical(x$weight_results, list())
  expect_identical(x$include_results, list())
  expect_identical(x$id, "solution1")
  expect_is(x$get_theme_results_data(), "data.frame")
  expect_is(x$get_weight_results_data(), "data.frame")
  expect_is(x$get_include_results_data(), "data.frame")
  expect_is(x$render_theme_results(), "datatables")
  expect_is(x$render_weight_results(), "datatables")
  expect_is(x$render_include_results(), "datatables")
  expect_identical(x$downloadable, TRUE)
})

test_that("initialization (from Result object)", {
  skip_on_ci()
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 6)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend()
  )
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend()
  )
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend()
  )
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend()
  )
  v6 <- new_variable(
    dataset = d, index = 6, total = 90, units = "ha",
    legend = simulate_exclude_legend()
  )  
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = 90, status = TRUE, id = "W1"
  )
  ## create an include using dataset
  incl <- new_include(
    name = "Protected areas", variable = v5,
    status = FALSE, id = "I1"
  )
  ## create an exclude using dataset
  excl <- new_exclude(
    name = "Urban areas", variable = v6,
    status = FALSE, id = "E1"
  )  
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    goal = 0.2, status = FALSE, current = 0.5, id = "F1"
  )
  f2 <- new_feature(
    name = "Forests", variable = v3,
    goal = 0.3, status = FALSE, current = 0.9, id = "F2"
  )
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    goal = 0.6, status = TRUE, current = 0.4, id = "F3"
  )
  ## create themes using the features
  t1 <- new_theme("Species", f1, id = "T1")
  t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ## create solution setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create result
  r <- min_set_result(
    id = "R1",
    area_data = d$get_planning_unit_areas(),
    boundary_data = d$get_boundary_data(),
    theme_data = ss$get_theme_data(),
    weight_data = ss$get_weight_data(),
    include_data = ss$get_include_data(),
    exclude_data = ss$get_exclude_data(),
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    exclude_settings = ss$get_exclude_settings(),
    parameters = ss$parameters,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status
  )
  ## create object
  x <- new_solution_from_result(
    id = "S1",
    result = r,
    name = "sol",
    visible = TRUE,
    downloadable = TRUE,
    dataset = d,
    settings = ss,
    legend = new_manual_legend(
      values = c(0, 1),
      colors = c("#00FFFF00", "#112233FF"),
      labels = c("not selected", "selected")
    )
  )
  # run tests
  expect_is(x, "Solution")
})

test_that("initialization (from Result object), sf", {
  skip_on_ci()
  # create object
  ## create dataset
  spatial_data <- import_simple_vector_data()
  idx <- seq_len(nrow(spatial_data))
  attribute_data <- tibble::tibble(
    V1 = runif(length(idx)),
    V2 = runif(length(idx)),
    V3 = runif(length(idx)),
    V4 = runif(length(idx)),
    V5 = runif(length(idx)),
    V6 = runif(length(idx)),
    `_index` = idx
  )
  # merge attribute data with spatial data
  sf_project <- merge(spatial_data, attribute_data, by.x = "id", by.y = "_index")
  # move id column to last position
  sf_project <- dplyr::relocate(sf_project, id, .after = last_col())
  # change id column name to _index
  names(sf_project)[names(sf_project) == "id"] <- "_index"
  
  # create object 
  d <- new_dataset_from_auto(
    x = sf_project
  )
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = new_null_legend()
  )
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = new_null_legend()
  )
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = new_null_legend()
  )
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = new_null_legend()
  )
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = new_null_legend()
  )
  v6 <- new_variable(
    dataset = d, index = 6, total = 90, units = "ha",
    legend = new_null_legend()
  )  
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = 90, status = TRUE, id = "W1"
  )
  ## create an include using dataset
  incl <- new_include(
    name = "Protected areas", variable = v5,
    status = FALSE, id = "I1"
  )
  ## create an exclude using dataset
  excl <- new_exclude(
    name = "Urban areas", variable = v6,
    status = FALSE, id = "E1"
  )  
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    goal = 0.2, status = FALSE, current = 0.5, id = "F1"
  )
  f2 <- new_feature(
    name = "Forests", variable = v3,
    goal = 0.3, status = FALSE, current = 0.9, id = "F2"
  )
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    goal = 0.6, status = TRUE, current = 0.4, id = "F3"
  )
  ## create themes using the features
  t1 <- new_theme("Species", f1, id = "T1")
  t2 <- new_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ## create solution setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create result
  r <- min_set_result(
    id = "R1",
    area_data = d$get_planning_unit_areas(),
    boundary_data = d$get_boundary_data(),
    theme_data = ss$get_theme_data(),
    weight_data = ss$get_weight_data(),
    include_data = ss$get_include_data(),
    exclude_data = ss$get_exclude_data(),    
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    exclude_settings = ss$get_exclude_settings(),    
    parameters = ss$parameters,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = 0
  )
  ## create object
  x <- new_solution_from_result(
    id = "S1",
    result = r,
    name = "sol",
    visible = TRUE,
    downloadable = TRUE,
    dataset = d,
    settings = ss,
    legend = new_manual_legend(
      values = c(0, 1),
      colors = c("#00FFFF00", "#112233FF"),
      labels = c("not selected", "selected")
    )
  )
  # run tests
  expect_equal(is.na(r$perimeter), FALSE)
  expect_equal(anyNA(x$get_summary_results_data()), FALSE)
  expect_is(x, "Solution")
})

test_that("get methods", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v2 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  f <- new_feature(
    name = "F1",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
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
  p <- new_parameter("budget", value = 12)
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x <- new_solution(
    name = "solution001",
    variable = v2,
    visible = TRUE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    exclude_results = list(),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), TRUE)
  expect_equal(x$get_invisible(), NA_real_)
  expect_equal(x$get_loaded(), TRUE)
})

test_that("set methods", {
  # create object
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 3)
  names(rd) <- c("a", "b", "solution_1")
  d <- new_dataset_from_auto(rd)
  v1 <- new_variable(
    d,
    index = 2, total = 89, units = "ha",
    legend = new_continuous_legend(
      min_value = 0, max_value = 20, colors = c("#FFFFFF", "#445566")
    )
  )
  v2 <- new_variable(
    d,
    index = "solution_1", total = 12, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  f <- new_feature(
    name = "F1",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    goal = 0.2,
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
  p <- new_parameter("budget", value = 12)
  x <- new_solution(
    name = "solution001",
    variable = v2,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(),
    include_results = list(),
    exclude_results = list(),
    id = "solution1"
  )
  # run tests
  expect_equal(x$get_visible(), FALSE)
  x$set_visible(TRUE)
  expect_equal(x$get_visible(), TRUE)
})

test_that("widget methods", {
  # create object
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  names(rd) <- c("a", "b", "solution_1", "c", "d")
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
  v4 <- new_variable(
    d,
    index = 3, total = 90, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )
  v5 <- new_variable(
    d,
    index = 4, total = 20, units = "ha",
    legend = new_categorical_legend(
      values = c(0, 1), colors = c("#FFFFFF", "#000000")
    )
  )  
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v1,
    visible = FALSE,
    status = FALSE,
    factor = -0.2,
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
  i <- new_include(
    name = "Protected areas",
    variable = v4,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  ir <- new_include_results(
    include = i,
    held = 0.78,
    id = "IID1"
  )
  excl <- new_exclude(
    name = "Highways",
    variable = v5,
    visible = FALSE,
    status = FALSE,
    id = "FID1"
  )
  exclr <- new_exclude_results(
    exclude = excl,
    held = 0.78,
    id = "EID1"
  )  
  p <- new_parameter("budget", value = 12)
  s1 <- new_statistic("Area", 12, "ha")
  s2 <- new_statistic("Perimeter", 10, "km")
  x_nohide <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    exclude_results = list(exclr),
    id = "solution1",
    hidden = FALSE
  )
  
  x_hide <- new_solution(
    name = "solution001",
    variable = v3,
    visible = FALSE,
    parameters = list(p),
    statistics = list(s1, s2),
    theme_results = list(thr),
    weight_results = list(wr),
    include_results = list(ir),
    exclude_results = list(exclr),
    id = "solution1",
    hidden = TRUE
  )  
  
  # run tests
  ## solution results widget
  expect_identical(
    x_nohide$get_solution_results_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      parameters = list(p$get_widget_data()),
      statistics = list(s1$get_widget_data(), s2$get_widget_data()),
      theme_results = list(thr$get_widget_data()),
      weight_results = list(wr$get_widget_data()),
      include_results = list(ir$get_widget_data()),
      exclude_results = list(exclr$get_widget_data()),
      solution_color = scales::alpha(last(x_nohide$variable$legend$colors), 1)
    )
  )
  ## map manager widget
  expect_identical(
    x_nohide$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      visible = FALSE,
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution",
      hidden = FALSE
    )
  )
  expect_identical(
    x_hide$get_map_manager_widget_data(),
    list(
      id = "solution1",
      name = "solution001",
      visible = FALSE,
      legend = v3$legend$get_widget_data(),
      units = v3$units,
      type = "solution",
      hidden = TRUE
    )
  )  
  
})
