context("new_map_manager")

test_that("initialization", {
  # create object
  ## create datasets
  l1 <- new_dataset(
    source = tempfile(), total = 12, units = "ha",
    legend = simulate_continuous_legend())
  l2 <- new_dataset(
    source = tempfile(), total = 14, units = "ha",
    legend = simulate_continuous_legend())
  l3 <- new_dataset(
    source = tempfile(), total = 78, units = "ha",
    legend = simulate_continuous_legend())
  l4 <- new_dataset(
    source = tempfile(), total = 90, units = "ha",
    legend = simulate_continuous_legend())
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2,
    initial_status = FALSE, initial_visible = TRUE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3,
    initial_status = FALSE, initial_visible = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6,
    initial_status = TRUE, initial_visible = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3))
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$layers, list(t1, t2, w))
  expect_identical(x$ids, c("T1", "T2", "W1"))
  expect_identical(x$order, c(2, 1, 3))
})

test_that("get methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(
    source = tempfile(), total = 12, units = "ha",
    legend = simulate_continuous_legend())
  l2 <- new_dataset(
    source = tempfile(), total = 14, units = "ha",
    legend = simulate_continuous_legend())
  l3 <- new_dataset(
    source = tempfile(), total = 78, units = "ha",
    legend = simulate_continuous_legend())
  l4 <- new_dataset(
    source = tempfile(), total = 90, units = "ha",
    legend = simulate_continuous_legend())
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2,
    initial_status = FALSE, initial_visible = TRUE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3,
    initial_status = FALSE, initial_visible = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6,
    initial_status = TRUE, initial_visible = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3))
  # run tests
  ## get layer
  expect_equal(x$get_layer("T1"), t1)
  expect_equal(x$get_layer("T2"), t2)
  expect_equal(x$get_order(), x$order)
  expect_equal(
    x$get_parameter(list(parameter = "order")),
    x$get_order())
  expect_equal(
    x$get_parameter(list(id = "W1", parameter = "visible")),
    w$get_visible())
  expect_equal(
    x$get_parameter(list(id = "T1", parameter = "feature_visible")),
    t1$get_feature_visible())
  expect_equal(
    x$get_parameter(list(id = "T2", parameter = "feature_visible")),
    t2$get_feature_visible())
  expect_equal(
    x$get_parameter(list(id = "T2", parameter = "feature_order")),
    t2$get_feature_order())
})

test_that("set methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(
    source = tempfile(), total = 12, units = "ha",
    legend = simulate_continuous_legend())
  l2 <- new_dataset(
    source = tempfile(), total = 14, units = "ha",
    legend = simulate_continuous_legend())
  l3 <- new_dataset(
    source = tempfile(), total = 78, units = "ha",
    legend = simulate_continuous_legend())
  l4 <- new_dataset(
    source = tempfile(), total = 90, units = "ha",
    legend = simulate_continuous_legend())
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2,
    initial_status = FALSE, initial_visible = TRUE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3,
    initial_status = FALSE, initial_visible = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6,
    initial_status = TRUE, initial_visible = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3))
  # run tests
  ## order
  x$set_parameter(
    list(parameter = "order", value = c(1, 3, 2)))
  expect_equal(
    x$get_order(), c(1, 3, 2))
  ## visible
  x$set_parameter(
    list(id = "W1", parameter = "visible",  value = FALSE))
  expect_equal(
    x$get_parameter(list(id = "W1", parameter = "visible")),
    FALSE)
  ## feature_visible
  x$set_parameter(
    list(id = "T1", parameter = "feature_visible",  value = TRUE))
  expect_equal(
    x$get_parameter(list(id = "T1", parameter = "feature_visible")),
    TRUE)
  x$set_parameter(
    list(id = "T2", parameter = "feature_visible",  value = c(FALSE, FALSE)))
  expect_equal(
    x$get_parameter(list(id = "T2", parameter = "feature_visible")),
    c(FALSE, FALSE))
  ## feature_order
  x$set_parameter(
    list(id = "T2", parameter = "feature_order",  value = c(3, 2)))
  expect_equal(
    x$get_parameter(list(id = "T2", parameter = "feature_order")),
    c(3, 2))
})

test_that("widget methods", {
  # create object
  ## create datasets
  l1 <- new_dataset(
    source = tempfile(), total = 12, units = "ha",
    legend = simulate_continuous_legend())
  l2 <- new_dataset(
    source = tempfile(), total = 14, units = "ha",
    legend = simulate_continuous_legend())
  l3 <- new_dataset(
    source = tempfile(), total = 78, units = "ha",
    legend = simulate_continuous_legend())
  l4 <- new_dataset(
    source = tempfile(), total = 90, units = "ha",
    legend = simulate_continuous_legend())
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", initial_factor = 90, initial_status = FALSE,
    dataset = l1, id = "W1")
  ## create features using datasets
  f1 <- new_feature(
    name = "Possum", initial_goal = 0.2,
    initial_status = FALSE, initial_visible = TRUE,
    current = 0.5, dataset = l2, id = "F1")
  f2 <- new_feature(
    name = "Forests", initial_goal = 0.3,
    initial_status = FALSE, initial_visible = FALSE,
     current = 0.9, dataset = l3, id = "F2")
  f3 <- new_feature(
    name = "Shrubs", initial_goal = 0.6,
    initial_status = TRUE, initial_visible = TRUE,
    current = 0.4, dataset = l4, id = "F3")
  ## create themes using the features
  t1 <- new_single_theme("Species", f1, id = "T1")
  t2 <- new_multi_theme("Ecoregions", list(f2, f3), id = "T2")
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      ids = c("T1", "T2", "W1"),
      order = c(2, 1, 3),
      layers = lapply(x$layers, function(x) x$get_map_manager_widget_data())
    )
  )
})
