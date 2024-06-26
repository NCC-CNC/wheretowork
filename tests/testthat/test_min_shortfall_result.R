context("min_shortfall_result")

test_that("no spatial clustering", {
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
    factor = -90, status = TRUE, id = "W1"
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
  ## create result setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create cache
  cache <- cachem::cache_mem()
  ## create object (run analysis without using cache)
  x1 <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status,
    cache = cache
  )
  ## create object (run analysis and use results from cache)
  x2 <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status,
    cache = cache
  )
  # run tests
  expect_is(x1, "Result")
  expect_is(x2, "Result")
  expect_equal(x1$values, x2$values)
})

test_that("spatial clustering", {
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
    factor = -90, status = TRUE, id = "W1"
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
  p1 <- new_parameter("Spatial clustering", value = 30, id = "P1")
  p2 <- new_parameter("Gap", id = "P2")
  ## create result setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create object
  x <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap =
      (ss$get_parameter("P1")$value * ss$get_parameter("P1")$status) / 100
  )
  # run tests
  expect_is(x, "Result")
})

test_that("no includes", {
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
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_exclude_legend()
  )  
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = TRUE, id = "W1"
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
  ## create result setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create object
  x <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status
  )
  # run tests
  expect_is(x, "Result")
})

test_that("no excludes", {
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
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_exclude_legend()
  )  
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = TRUE, id = "W1"
  )
  ## create an include using dataset
  incl <- new_include(
    name = "Protected areas", variable = v5,
    status = FALSE, id = "I1"
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
  ## create result setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    excludes = list(), parameters = list(p1, p2)
  )
  ## create object
  x <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
  # run tests
  expect_is(x, "Result")
})

test_that("no weights", {
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
  ## create result setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create object
  x <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status
  )
  # run tests
  expect_is(x, "Result")
})

test_that("mixed weights", {
  skip_on_ci()
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 7)
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
    legend = simulate_include_legend()
  )
  v7 <- new_variable(
    dataset = d, index = 6, total = 90, units = "ha",
    legend = simulate_exclude_legend()
  )  
  ## create a weight using dataset
  w1 <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = TRUE, id = "W1"
  )
  w2 <- new_weight(
    name = "Naturalness", variable = v5,
    factor = 90, status = TRUE, id = "W2"
  )
  ## create an include using dataset
  incl <- new_include(
    name = "Protected areas", variable = v6,
    status = FALSE, id = "I1"
  )
  ## create an exclude using dataset
  excl <- new_exclude(
    name = "Urban areas", variable = v7,
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
    themes = list(t1, t2), weights = list(w1, w2), includes = list(incl),
    excludes = list(excl), parameters = list(p1, p2)
  )
  ## create object
  x <- min_shortfall_result(
    id = "R1",
    area_budget_proportion = 0.7,
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
    overlap = FALSE,
    gap_1 = ss$get_parameter("P2")$value * ss$get_parameter("P2")$status,
    boundary_gap = ss$get_parameter("P1")$value * ss$get_parameter("P1")$status
  )
  # run tests
  expect_is(x, "Result")
})

