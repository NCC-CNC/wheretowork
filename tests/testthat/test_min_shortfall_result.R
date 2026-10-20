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

# helper function to create inputs for the weight tests
create_weight_test_inputs <- function(factors) {
  ## create dataset
  rd <- simulate_continuous_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  totals <- terra::global(rd, "sum", na.rm = TRUE)[[1]]
  v <- lapply(seq_len(5), function(i) {
    new_variable(
      dataset = d, index = i, total = totals[[i]], units = "ha",
      legend = simulate_continuous_legend()
    )
  })
  ## create weights
  w <- lapply(seq_along(factors), function(i) {
    new_weight(
      name = paste("Weight", i), variable = v[[i]],
      factor = factors[[i]], status = TRUE, id = paste0("W", i)
    )
  })
  ## create features
  f1 <- new_feature(
    name = "Possum", variable = v[[4]],
    goal = 0.2, status = TRUE, current = 0, id = "F1"
  )
  f2 <- new_feature(
    name = "Forests", variable = v[[5]],
    goal = 0.3, status = TRUE, current = 0, id = "F2"
  )
  ## create solution settings
  ss <- new_solution_settings(
    themes = list(new_theme("Species", list(f1, f2), id = "T1")),
    weights = w, includes = list(), excludes = list(),
    parameters = list(new_parameter("Spatial clustering", id = "P1"))
  )
  list(d = d, ss = ss)
}

# helper function to run min_shortfall_result with weight test inputs
run_weight_test <- function(x, area_budget_proportion, cache, boundary_gap = 0,
                            ...) {
  min_shortfall_result(
    id = "R1",
    area_budget_proportion = area_budget_proportion,
    area_data = x$d$get_planning_unit_areas(),
    boundary_data = x$d$get_boundary_data(),
    theme_data = x$ss$get_theme_data(),
    weight_data = x$ss$get_weight_data(),
    include_data = x$ss$get_include_data(),
    exclude_data = x$ss$get_exclude_data(),
    theme_settings = x$ss$get_theme_settings(),
    weight_settings = x$ss$get_weight_settings(),
    include_settings = x$ss$get_include_settings(),
    exclude_settings = x$ss$get_exclude_settings(),
    parameters = x$ss$parameters,
    boundary_gap = boundary_gap,
    cache = cache,
    ...
  )
}

test_that("positive and negative weights with area budget (#414)", {
  skip_on_ci()
  # create object
  x <- create_weight_test_inputs(c(100, -100))
  r1 <- run_weight_test(x, 0.1, cachem::cache_mem())
  r2 <- run_weight_test(x, 0.1, cachem::cache_mem(), boundary_gap = 0.2)
  # run tests
  expect_is(r1, "Result")
  expect_is(r2, "Result")
})

test_that("negative weights do not restrict area budget (#446)", {
  skip_on_ci()
  # create object
  ## note that the area budget is too small to meet the goals
  x <- create_weight_test_inputs(c(-99, -99, -99))
  r <- run_weight_test(x, 0.1, cachem::cache_mem())
  # run tests
  area <- x$d$get_planning_unit_areas()
  ts <- x$ss$get_theme_settings()
  held <- as.numeric(x$ss$get_theme_data() %*% r$values)
  expect_true(any(held < ts$goal * ts$total))
  expect_gte(sum(r$values * area), 0.1 * sum(area) - max(area))
  expect_lte(sum(r$values * area), 0.1 * sum(area) + 1e-6)
})

test_that("min_budget_proportion", {
  skip_on_ci()
  # create object
  ## note that the area budget is too small to meet the goals
  x <- create_weight_test_inputs(c(-99, -99, -99))
  r1 <- run_weight_test(x, 0.1, cachem::cache_mem())
  r2 <- run_weight_test(
    x, 0.1, cachem::cache_mem(), min_budget_proportion = 0.8
  )
  # run tests
  area <- x$d$get_planning_unit_areas()
  expect_gte(sum(r1$values * area), 0.99 * 0.1 * sum(area) - max(area))
  expect_lte(sum(r1$values * area), 0.1 * sum(area) + 1e-6)
  expect_gte(sum(r2$values * area), 0.8 * 0.1 * sum(area) - max(area))
  expect_lte(sum(r2$values * area), 0.1 * sum(area) + 1e-6)
})

test_that("min_budget_proportion not used when budget is larger than needed", {
  skip_on_ci()
  # create object
  ## note that the area budget is large enough to meet the goals
  x <- create_weight_test_inputs(c(-99))
  r <- run_weight_test(x, 0.8, cachem::cache_mem())
  # run tests
  area <- x$d$get_planning_unit_areas()
  ts <- x$ss$get_theme_settings()
  held <- as.numeric(x$ss$get_theme_data() %*% r$values)
  expect_true(all(held >= ts$goal * ts$total - 1e-6))
  expect_lt(sum(r$values * area), 0.99 * 0.8 * sum(area))
})

test_that("weights prefer high values and avoid low values", {
  skip_on_ci()
  # create object
  x <- create_weight_test_inputs(c(100, -100))
  r <- run_weight_test(x, 0.3, cachem::cache_mem())
  # run tests
  wd <- x$ss$get_weight_data()
  selected <- r$values > 0.5
  expect_gt(mean(wd[1, selected]), mean(wd[1, ]))
  expect_lt(mean(wd[2, selected]), mean(wd[2, ]))
})

test_that("weight_importance", {
  skip_on_ci()
  # create object
  ## note that the area budget is too small to meet the goals
  x <- create_weight_test_inputs(c(100))
  r1 <- run_weight_test(x, 0.1, cachem::cache_mem(), weight_importance = 0.01)
  r2 <- run_weight_test(x, 0.1, cachem::cache_mem(), weight_importance = 1)
  # run tests
  ts <- x$ss$get_theme_settings()
  td <- x$ss$get_theme_data()
  wd <- x$ss$get_weight_data()
  shortfall <- function(r) {
    held <- as.numeric(td %*% r$values)
    sum(pmax(ts$goal * ts$total - held, 0) / (ts$goal * ts$total))
  }
  ## higher importance holds more of the weight, at the expense of the goals
  expect_gt(sum(wd[1, ] * r2$values), sum(wd[1, ] * r1$values))
  expect_gt(shortfall(r2), shortfall(r1))
  ## values outside (0, 1] are not supported
  expect_error(
    run_weight_test(x, 0.1, cachem::cache_mem(), weight_importance = 0)
  )
  expect_error(
    run_weight_test(x, 0.1, cachem::cache_mem(), weight_importance = 2)
  )
})

test_that("objective scaling values are cached", {
  skip_on_ci()
  # create object
  x <- create_weight_test_inputs(c(50, -50))
  cache <- cachem::cache_mem()
  r1 <- run_weight_test(x, 0.3, cache)
  n1 <- length(cache$keys())
  x$ss$weights[[1]]$set_factor(80)
  r2 <- run_weight_test(x, 0.3, cache)
  n2 <- length(cache$keys())
  # run tests
  expect_is(r1, "Result")
  expect_is(r2, "Result")
  ## solution and best value for the primary problem
  expect_equal(n1, 2)
  ## only a new solution is cached, since the primary problem is reused
  expect_equal(n2, 3)
})

test_that("spatial clustering is not reused from the cache", {
  skip_on_ci()
  # create object
  x <- create_weight_test_inputs(c(50))
  x$ss$weights[[1]]$set_status(FALSE)
  cache <- cachem::cache_mem()
  r1 <- run_weight_test(x, 0.3, cache, boundary_gap = 0)
  n1 <- length(cache$keys())
  r2 <- run_weight_test(x, 0.3, cache, boundary_gap = 0.5)
  n2 <- length(cache$keys())
  r3 <- run_weight_test(x, 0.3, cachem::cache_mem(), boundary_gap = 0.5)
  # run tests
  ## spatial clustering reduces the budget for the initial solution,
  ## so a new initial solution is generated
  expect_equal(n1, 1)
  expect_equal(n2, 2)
  expect_equal(r2$values, r3$values)
})

test_that("calculate_linear_objective_max", {
  # create data
  values <- c(10, 8, 1, -2, -5)
  cost <- c(1, 2, 1, 1, 1)
  locked_in <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  locked_out <- c(FALSE, FALSE, FALSE, FALSE, FALSE)
  # run tests
  ## uses best value per cost until the budget is used
  expect_equal(
    calculate_linear_objective_max(values, cost, 3, 2, locked_in, locked_out),
    10 + 8
  )
  ## partial planning units are selected to use the budget
  expect_equal(
    calculate_linear_objective_max(values, cost, 2, 1.5, locked_in, locked_out),
    10 + 4
  )
  ## planning units that reduce the objective are not selected once
  ## the minimum budget is met
  expect_equal(
    calculate_linear_objective_max(-values, cost, 3, 2, locked_in, locked_out),
    5 + 2
  )
  ## otherwise they are selected to meet the minimum budget,
  ## starting with the least bad
  expect_equal(
    calculate_linear_objective_max(
      c(-1, -2, -3), c(1, 1, 1), 3, 2, rep(FALSE, 3), rep(FALSE, 3)
    ),
    -1 + -2
  )
  ## no planning units that reduce the objective are selected
  ## without a minimum budget
  expect_equal(
    calculate_linear_objective_max(
      c(-1, -2, -3), c(1, 1, 1), 3, 0, rep(FALSE, 3), rep(FALSE, 3)
    ),
    0
  )
  ## locked in/out planning units are accounted for
  expect_equal(
    calculate_linear_objective_max(
      values, cost, 3, 2,
      c(FALSE, FALSE, FALSE, FALSE, TRUE),
      c(TRUE, FALSE, FALSE, FALSE, FALSE)
    ),
    -5 + 8
  )
})
