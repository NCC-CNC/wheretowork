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
  x <- new_map_manager(
    layers = list(t1, t2, w),
    visible = c(TRUE, FALSE, TRUE),
    order = c(2, 1, 3))
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$layers, list(t1, t2, w))
  expect_identical(x$ids, c("T1", "T2", "W1"))
  expect_identical(x$visible, c(TRUE, FALSE, TRUE))
  expect_identical(x$order, c(2, 1, 3))
  expect_identical(x$sub_order, list(1, c(2, 1), 1))
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
  x <- new_map_manager(
    layers = list(t1, t2, w),
    visible = c(TRUE, FALSE, TRUE),
    order = c(2, 1, 3))
  # run tests
  ## get layer
  expect_equal(x$get_layer("T1"), t1)
  expect_equal(x$get_layer("T2"), t2)
  expect_equal(x$get_visible(), x$visible)
  expect_equal(x$get_order(), x$order)
  expect_equal(x$get_sub_order(), x$sub_order)
  expect_equal(
    x$get_parameter(list(parameter = "order")),
    x$get_order())
  expect_equal(
    x$get_parameter(list(parameter = "visible")),
    x$get_visible())
  expect_equal(
    x$get_parameter(list(parameter = "sub_order")),
    x$get_sub_order())
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
  x <- new_map_manager(
    layers = list(t1, t2, w),
    visible = c(TRUE, FALSE, TRUE),
    order = c(2, 1, 3))
  # run tests
  ## visible
  x$set_parameter(
    list(parameter = "visible", value = c(FALSE, TRUE, TRUE)))
  expect_equal(
    x$get_visible(), c(FALSE, TRUE, TRUE))
  ## order
  x$set_parameter(
    list(parameter = "order", value = c(1, 3, 2)))
  expect_equal(
    x$get_order(), c(1, 3, 2))
  ## order
  x$set_parameter(
    list(parameter = "sub_order", id = "T1", value = 5))
  expect_equal(
    x$get_sub_order(), list(5, c(2, 1), 1))
  x$set_parameter(
    list(parameter = "sub_order", id = "T2", value = c(1, 2)))
  expect_equal(
    x$get_sub_order(), list(5, c(1, 2), 1))
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
  x <- new_map_manager(
    layers = list(t1, t2, w),
    visible = c(TRUE, FALSE, TRUE),
    order = c(2, 1, 3))
  # run tests
  expect_equal(
    x$get_widget_data(),
    list(
      ids = c("T1", "T2", "W1"),
      visible = c(TRUE, FALSE, TRUE),
      order = c(2, 1, 3),
      layers = lapply(x$layers, function(x) x$get_map_manager_widget_data())
    )
  )
})
