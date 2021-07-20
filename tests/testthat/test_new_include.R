context("new_include")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_include_legend())
  x <- new_include(
    name = "National protected areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    status = FALSE,
    id = "FID1")
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "National protected areas")
  expect_identical(x$mandatory, TRUE)
  expect_identical(x$variable, v)
  expect_identical(x$visible, FALSE)
  expect_identical(x$status, FALSE)
  expect_identical(x$id, "FID1")
})

test_that("get methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_include_legend())
  x <- new_include(
    name = "National protected areas",
    variable = v,
    mandatory = TRUE,
    visible = TRUE,
    status = FALSE,
    id = "FID1")
  # run tests
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_visible(), TRUE)
})

test_that("set methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_include_legend())
  x <- new_include(
    name = "National protected areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    status = FALSE,
    id = "FID1")
  # run tests
  x$set_status(TRUE)
  x$set_visible(FALSE)
  expect_identical(x$get_status(), TRUE)
  expect_identical(x$get_visible(), FALSE)
})

test_that("export method", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_include_legend())
  x <- new_include(
    name = "National protected areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    status = FALSE,
    id = "FID1")
  # run tests
  expect_identical(
    x$export(),
    list(
      name = "National protected areas",
      variable = x$variable$export(),
      mandatory = TRUE,
      status = FALSE,
      visible = FALSE
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_include_legend())
  x <- new_include(
    name = "National protected areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    status = FALSE,
    id = "FID1")
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FID1",
      name = "National protected areas",
      status = FALSE,
      mandatory = TRUE)
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FID1",
      name = "National protected areas",
      visible = FALSE,
      legend = v$legend$get_widget_data(),
      units = "",
      type = "include")
  )
})
