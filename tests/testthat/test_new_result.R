context("new_result")

test_that("initialization", {
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
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
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = 90, status = TRUE, id = "W1"
  )
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
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
  ## create solution setting
  ss <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1, p2)
  )
  ## create values
  v <- sample(c(0, 1), 10, replace = TRUE)
  ## create object
  r <- new_result(
    id = "R1",
    values = v,
    area = 4,
    perimeter = 12,
    theme_coverage = c("F1" = 0.1, "F2" = 0.2, "F3" = 0.3),
    weight_coverage = c("W1" = 0.4),
    include_coverage = c("I1" = 0.5),
    theme_settings = ss$get_theme_settings(),
    weight_settings = ss$get_weight_settings(),
    include_settings = ss$get_include_settings(),
    parameters = list(p1, p2)
  )
  # run tests
  expect_identical(r$id, "R1")
  expect_identical(r$values, v)
  expect_identical(r$area, 4)
  expect_identical(r$perimeter, 12)
  expect_identical(r$theme_coverage, c("F1" = 0.1, "F2" = 0.2, "F3" = 0.3))
  expect_identical(r$weight_coverage, c("W1" = 0.4))
  expect_identical(r$include_coverage, c("I1" = 0.5))
  expect_identical(r$theme_settings, ss$get_theme_settings())
  expect_identical(r$weight_settings, ss$get_weight_settings())
  expect_identical(r$include_settings, ss$get_include_settings())
  expect_equal(r$parameters, list(p1, p2))
})
