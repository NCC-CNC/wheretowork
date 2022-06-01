context("new_map_manager")

test_that("initialization", {
  skip_if_not_installed("RandomFields")
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 4)
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
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = FALSE, id = "W1"
  )
  ## create features using datasets
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
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3)
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$layers, list(t1, t2, w))
  expect_identical(x$ids, c("T1", "T2", "W1"))
  expect_identical(x$order, c(2, 1, 3))
  expect_equal(
    x$get_layer_names(),
    c("Possum", "Forests", "Shrubs", "Human Footprint Index")
  )
  expect_equal(
    x$get_layer_indices(),
    names(rd)[c(2, 3, 4, 1)]
  )
})

test_that("get methods", {
  skip_if_not_installed("RandomFields")
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 4)
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
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = FALSE, id = "W1"
  )
  ## create features using datasets
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
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3)
  )
  # run tests
  ## get layer
  expect_equal(x$get_layer("T1"), t1)
  expect_equal(x$get_layer("T2"), t2)
  expect_equal(x$get_order(), x$order)
  expect_equal(
    x$get_setting(list(setting = "order")),
    x$get_order()
  )
  expect_equal(
    x$get_setting(list(id = "W1", setting = "visible")),
    w$get_visible()
  )
  expect_equal(
    x$get_setting(list(id = "T1", setting = "feature_visible")),
    t1$get_feature_visible()
  )
  expect_equal(
    x$get_setting(list(id = "T2", setting = "feature_visible")),
    t2$get_feature_visible()
  )
  expect_equal(
    x$get_setting(list(id = "T2", setting = "feature_order")),
    t2$get_feature_order()
  )
})

test_that("set methods", {
  skip_if_not_installed("RandomFields")
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 4)
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
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = FALSE, id = "W1"
  )
  ## create features using datasets
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
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3)
  )
  # run tests
  ## order
  x$set_setting(
    list(setting = "order", value = c(1, 3, 2))
  )
  expect_equal(
    x$get_order(), c(1, 3, 2)
  )
  ## visible
  x$set_setting(
    list(id = "W1", setting = "visible", value = FALSE)
  )
  expect_equal(
    x$get_setting(list(id = "W1", setting = "visible")),
    FALSE
  )
  ## feature_visible
  x$set_setting(
    list(id = "T1", setting = "feature_visible", value = TRUE)
  )
  expect_equal(
    x$get_setting(list(id = "T1", setting = "feature_visible")),
    TRUE
  )
  x$set_setting(
    list(id = "T2", setting = "feature_visible", value = c(FALSE, FALSE))
  )
  expect_equal(
    x$get_setting(list(id = "T2", setting = "feature_visible")),
    c(FALSE, FALSE)
  )
  ## feature_order
  x$set_setting(
    list(id = "T2", setting = "feature_order", value = c(3, 2))
  )
  expect_equal(
    x$get_setting(list(id = "T2", setting = "feature_order")),
    c(3, 2)
  )
  ## visible
  x$set_visible(FALSE)
  expect_false(f1$get_visible())
  expect_false(f2$get_visible())
  expect_false(f3$get_visible())
  expect_false(w$get_visible())
})

test_that("widget methods", {
  skip_if_not_installed("RandomFields")
  # create object
  ## create dataset
  rd <- simulate_binary_spatial_data(import_simple_raster_data(), 4)
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
  ## create a weight using a dataset
  w <- new_weight(
    name = "Human Footprint Index", variable = v1,
    factor = -90, status = FALSE, id = "W1"
  )
  ## create features using datasets
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
  ## create solution setting
  x <- new_map_manager(
    layers = list(t1, t2, w),
    order = c(2, 1, 3)
  )
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
