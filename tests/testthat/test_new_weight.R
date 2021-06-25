context("new_weight")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = FALSE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$id, "FID1")
  expect_equal(x$name, "Human Footprint Index")
  expect_equal(x$variable, v)
  expect_equal(x$visible, FALSE)
  expect_equal(x$initial_visible, FALSE)
  expect_equal(x$status, FALSE)
  expect_equal(x$initial_status, FALSE)
  expect_equal(x$factor, 0.2)
  expect_equal(x$initial_factor, 0.2)
  expect_equal(x$min_factor, 0.01)
  expect_equal(x$max_factor, 0.9)
  expect_equal(x$step_factor, 0.03)
})

test_that("get methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  expect_identical(x$get_factor(), 0.2)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_parameter("factor"), x$get_factor())
  expect_identical(x$get_parameter("status"), x$get_status())
})

test_that("set methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  x$set_factor(0.8)
  x$set_status(TRUE)
  x$set_visible(FALSE)
  expect_identical(x$get_factor(), 0.8)
  expect_identical(x$get_status(), TRUE)
  expect_identical(x$get_visible(), FALSE)
  x$set_parameter("factor", 0.3)
  x$set_parameter("status", FALSE)
  x$set_parameter("visible", TRUE)
  expect_identical(x$get_factor(), 0.3)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_visible(), TRUE)
})

test_that("export method", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_visible = TRUE,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  expect_equal(
    x$export(),
    list(
      name = "Human Footprint Index",
      variable = x$variable$export(),
      initial_status = FALSE,
      initial_visible = TRUE,
      initial_factor = 0.2,
      min_factor = 0.01,
      max_factor = 0.9,
      step_factor = 0.03
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend())
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    initial_status = FALSE,
    initial_factor = 0.2,
    min_factor = 0.01,
    max_factor = 0.9,
    step_factor = 0.03,
    id = "FID1")
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FID1",
      name = "Human Footprint Index",
      min_factor = 0.01,
      max_factor = 0.9,
      factor = 0.2,
      step_factor = 0.03,
      status = FALSE)
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FID1",
      name = "Human Footprint Index",
      visible = TRUE,
      legend = v$legend$get_widget_data(),
      units = "ha",
      type = "weight")
  )
})
