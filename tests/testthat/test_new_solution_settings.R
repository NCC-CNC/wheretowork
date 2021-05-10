context("new_solution_settings")

test_that("initialization", {
  # create object
  ## create datasets
  l1 <- new_dataset(source = tempfile(), total = 12, units = "ha")
  l2 <- new_dataset(source = tempfile(), total = 14, units = "ha")
  l3 <- new_dataset(source = tempfile(), total = 78, units = "ha")
  l4 <- new_dataset(source = tempfile(), total = 90, units = "ha")
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2, initial_status = FALSE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3, initial_status = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6, initial_status = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_solution_settings(themes = list(t1, t2), weights = list(w))
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$themes, list(t1, t2))
  expect_equal(x$weights, list(w))
  expect_identical(x$theme_ids, c("T1", "T2"))
  expect_identical(x$weight_ids, "W1")
})

test_that("get methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(source = tempfile(), total = 12, units = "ha")
  l2 <- new_dataset(source = tempfile(), total = 14, units = "ha")
  l3 <- new_dataset(source = tempfile(), total = 78, units = "ha")
  l4 <- new_dataset(source = tempfile(), total = 90, units = "ha")
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2, initial_status = FALSE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3, initial_status = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6, initial_status = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_solution_settings(themes = list(t1, t2), weights = list(w))
  # run tests
  ## get theme
  expect_equal(x$get_theme("T1"), t1)
  expect_equal(x$get_theme("T2"), t2)
  ## get weight
  expect_equal(x$get_weight("W1"), w)
  ## invalid themes and weights
  expect_error(x$get_theme("NOT_EXISTANT"))
  expect_error(x$get_weight("NOT_EXISTANT"))
  ## get parameters for SingleTheme
  expect_equal(
    x$get_parameter(
      list(id = "T1", parameter = "feature_goal", type = "theme")),
      0.2)
  expect_equal(
    x$get_parameter(
      list(id = "T1", parameter = "feature_status", type = "theme")),
      FALSE)
  ## get parameters for MultiTheme
  expect_equal(
    x$get_parameter(
      list(id = "T2", parameter = "feature_goal", type = "theme")),
      c(0.3, 0.6))
  expect_equal(
    x$get_parameter(
      list(id = "T2", parameter = "feature_status", type = "theme")),
      c(FALSE, TRUE))
  ## get parameters for Weight
  expect_equal(
    x$get_parameter(
      list(id = "W1", parameter = "factor", type = "weight")),
      90)
  expect_equal(
    x$get_parameter(
      list(id = "W1", parameter = "status", type = "weight")),
      FALSE)
})

test_that("set methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(source = tempfile(), total = 12, units = "ha")
  l2 <- new_dataset(source = tempfile(), total = 14, units = "ha")
  l3 <- new_dataset(source = tempfile(), total = 78, units = "ha")
  l4 <- new_dataset(source = tempfile(), total = 90, units = "ha")
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2, initial_status = FALSE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3, initial_status = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6, initial_status = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_solution_settings(themes = list(t1, t2), weights = list(w))
  # run tests
  ## singleTheme
  x$set_parameter(
    list(id = "T1", parameter = "feature_status", value = TRUE, type = "theme"))
  expect_equal(
    x$get_parameter(
      list(id = "T1", parameter = "feature_status", type = "theme")),
    TRUE)
  x$set_parameter(
    list(id = "T1", parameter = "feature_goal", value = 0.91, type = "theme"))
  expect_equal(
    x$get_parameter(
      list(id = "T1", parameter = "feature_goal", type = "theme")),
    0.91)
  ## multiTheme
  x$set_parameter(
    list(
      id = "T2", parameter = "feature_status", value = c(TRUE, FALSE),
      type = "theme"))
  expect_equal(
    x$get_parameter(
      list(id = "T2", parameter = "feature_status", type = "theme")),
    c(TRUE, FALSE))
  x$set_parameter(
    list(
      id = "T2", parameter = "feature_goal", value = c(0.99, 0.21),
      type = "theme"))
  expect_equal(
    x$get_parameter(
      list(id = "T2", parameter = "feature_goal", type = "theme")),
    c(0.99, 0.21))
  ## Weight
  x$set_parameter(
    list(id = "W1", parameter = "status", value = TRUE, type = "weight"))
  expect_equal(
    x$get_parameter(
      list(id = "W1", parameter = "status", type = "weight")),
    TRUE)
  x$set_parameter(
    list(id = "W1", parameter = "factor", value = 90, type = "weight"))
  expect_equal(
    x$get_parameter(
      list(id = "W1", parameter = "factor", type = "weight")),
    90)
})

test_that("widget methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(source = tempfile(), total = 12, units = "ha")
  l2 <- new_dataset(source = tempfile(), total = 14, units = "ha")
  l3 <- new_dataset(source = tempfile(), total = 78, units = "ha")
  l4 <- new_dataset(source = tempfile(), total = 90, units = "ha")
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2, initial_status = FALSE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3, initial_status = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6, initial_status = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_solution_settings(themes = list(t1, t2), weights = list(w))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      themes = list(
        t1$get_solution_settings_widget_data(),
        t2$get_solution_settings_widget_data()),
      weights = list(
        w$get_solution_settings_widget_data()))
  )
})
