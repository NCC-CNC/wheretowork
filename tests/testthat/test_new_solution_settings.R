context("new_solution_settings")

test_that("initialization", {
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend())
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend())
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend())
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend())
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend())
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    initial_factor = 90, initial_status = FALSE, id = "W1")
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    initial_status = FALSE, id = "I1")
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
  f2 <- new_feature(
    name = "Forests", variable = v3,
    initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", id = "P1")
  ## create solution setting
  x <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1))
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$themes, list(t1, t2))
  expect_equal(x$weights, list(w))
  expect_equal(x$includes, list(incl))
  expect_identical(x$theme_ids, c("T1", "T2"))
  expect_identical(x$weight_ids, "W1")
  expect_identical(x$include_ids, "I1")
  expect_identical(x$parameter_ids, "P1")
})

test_that("get methods", {
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend())
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend())
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend())
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend())
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend())
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    initial_factor = 90, initial_status = FALSE, id = "W1")
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    initial_status = TRUE, id = "I1")
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
  f2 <- new_feature(
    name = "Forests", variable = v3,
    initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", initial_value = 90, id = "P1")
  ## create solution setting
  x <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1))
  # run tests
  ## get theme
  expect_equal(x$get_theme("T1"), t1)
  expect_equal(x$get_theme("T2"), t2)
  ## get weight
  expect_equal(x$get_weight("W1"), w)
  ## invalid themes and weights
  expect_error(x$get_theme("NOT_EXISTANT"))
  expect_error(x$get_weight("NOT_EXISTANT"))
  ## get settings for SingleTheme
  expect_equal(
    x$get_setting(
      list(id = "T1", setting = "feature_goal", type = "theme")),
      0.2)
  expect_equal(
    x$get_setting(
      list(id = "T1", setting = "feature_status", type = "theme")),
      FALSE)
  ## get settings for MultiTheme
  expect_equal(
    x$get_setting(
      list(id = "T2", setting = "feature_goal", type = "theme")),
      c(0.3, 0.6))
  expect_equal(
    x$get_setting(
      list(id = "T2", setting = "feature_status", type = "theme")),
      c(FALSE, TRUE))
  ## get settings for Weight
  expect_equal(
    x$get_setting(
      list(id = "W1", setting = "factor", type = "weight")),
      90)
  expect_equal(
    x$get_setting(
      list(id = "W1", setting = "status", type = "weight")),
      FALSE)
  ## get settings for Include
  expect_equal(
    x$get_setting(
      list(id = "I1", setting = "status", type = "include")),
    TRUE)
  ## get settings for Parameter
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "status", type = "parameter")),
    TRUE)
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "value", type = "parameter")),
    90)
})

test_that("set methods", {
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend())
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend())
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend())
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend())
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend())
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    initial_factor = 90, initial_status = FALSE, id = "W1")
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    initial_status = TRUE, id = "I1")
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
  f2 <- new_feature(
    name = "Forests", variable = v3,
    initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", initial_value = 90, id = "P1")
  ## create solution setting
  x <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1))
  # run tests
  ## singleTheme
  x$set_setting(
    list(id = "T1", setting = "feature_status", value = TRUE, type = "theme"))
  expect_equal(
    x$get_setting(
      list(id = "T1", setting = "feature_status", type = "theme")),
    TRUE)
  x$set_setting(
    list(id = "T1", setting = "feature_goal", value = 0.91, type = "theme"))
  expect_equal(
    x$get_setting(
      list(id = "T1", setting = "feature_goal", type = "theme")),
    0.91)
  ## multiTheme
  x$set_setting(
    list(
      id = "T2", setting = "feature_status", value = c(TRUE, FALSE),
      type = "theme"))
  expect_equal(
    x$get_setting(
      list(id = "T2", setting = "feature_status", type = "theme")),
    c(TRUE, FALSE))
  x$set_setting(
    list(
      id = "T2", setting = "feature_goal", value = c(0.99, 0.21),
      type = "theme"))
  expect_equal(
    x$get_setting(
      list(id = "T2", setting = "feature_goal", type = "theme")),
    c(0.99, 0.21))
  ## Weight
  x$set_setting(
    list(id = "W1", setting = "status", value = TRUE, type = "weight"))
  expect_equal(
    x$get_setting(
      list(id = "W1", setting = "status", type = "weight")),
    TRUE)
  x$set_setting(
    list(id = "W1", setting = "factor", value = 90, type = "weight"))
  expect_equal(
    x$get_setting(
      list(id = "W1", setting = "factor", type = "weight")),
    90)
  ## Include
  expect_equal(
    x$get_setting(
      list(id = "I1", setting = "status", type = "include")),
    TRUE)
  x$set_setting(
    list(id = "I1", setting = "status", value = FALSE, type = "include"))
  expect_equal(
    x$get_setting(
      list(id = "I1", setting = "status", type = "include")),
    FALSE)
  ## Parameter
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "status", type = "parameter")),
    TRUE)
  x$set_setting(
    list(id = "P1", setting = "status", value = FALSE, type = "parameter"))
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "status", type = "parameter")),
    FALSE)
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "value", type = "parameter")),
    90)
  x$set_setting(
    list(id = "P1", setting = "value", value = 12, type = "parameter"))
  expect_equal(
    x$get_setting(
      list(id = "P1", setting = "value", type = "parameter")),
    12)
})

test_that("widget methods", {
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 5)
  d <- new_dataset_from_auto(rd)
  ## create variables
  v1 <- new_variable(
    dataset = d, index = 1, total = 12, units = "ha",
    legend = simulate_continuous_legend())
  v2 <- new_variable(
    dataset = d, index = 2, total = 14, units = "ha",
    legend = simulate_continuous_legend())
  v3 <- new_variable(
    dataset = d, index = 3, total = 78, units = "ha",
    legend = simulate_continuous_legend())
  v4 <- new_variable(
    dataset = d, index = 4, total = 90, units = "ha",
    legend = simulate_continuous_legend())
  v5 <- new_variable(
    dataset = d, index = 5, total = 90, units = "ha",
    legend = simulate_include_legend())
  ## create a weight using dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    initial_factor = 90, initial_status = FALSE, id = "W1")
  ## create a weight using dataset
  incl <- new_include(
    name = "Protected areas", variable = v1,
    initial_status = TRUE, id = "I1")
  ## create features using dataset
  f1 <- new_feature(
    name = "Possum", variable = v2,
    initial_goal = 0.2, initial_status = FALSE, current = 0.5, id = "F1")
  f2 <- new_feature(
    name = "Forests", variable = v3,
    initial_goal = 0.3, initial_status = FALSE, current = 0.9, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", variable = v4,
    initial_goal = 0.6, initial_status = TRUE, current = 0.4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create parameter
  p1 <- new_parameter("Spatial clustering", initial_value = 90, id = "P1")
  ## create solution setting
  x <- new_solution_settings(
    themes = list(t1, t2), weights = list(w), includes = list(incl),
    parameters = list(p1))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      themes = list(
        t1$get_solution_settings_widget_data(),
        t2$get_solution_settings_widget_data()),
      weights = list(
        w$get_solution_settings_widget_data()),
      includes = list(
        incl$get_solution_settings_widget_data()),
      parameters = list(
        p1$get_solution_settings_widget_data())
    )
  )
})
