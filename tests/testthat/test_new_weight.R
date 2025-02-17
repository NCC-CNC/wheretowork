context("new_weight")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = FALSE,
    hidden = TRUE,
    status = FALSE,
    factor = -0.2,
    current = 0.89,
    id = "FID1",
    downloadable = TRUE
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_equal(x$id, "FID1")
  expect_equal(x$name, "Human Footprint Index")
  expect_equal(x$variable, v)
  expect_equal(x$visible, FALSE)
  expect_equal(x$invisible, NA_real_)
  expect_equal(x$loaded, FALSE)
  expect_equal(x$hidden, TRUE)
  expect_equal(x$status, FALSE)
  expect_equal(x$current, 0.89)
  expect_equal(x$factor, -0.2)
  expect_equal(x$downloadable, TRUE)
})

test_that("get methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    current = 0.89,
    factor = -0.2,
    id = "FID1",
    downloadable = TRUE
  )
  # run tests
  expect_identical(x$get_factor(), -0.2)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_invisible(), NA_real_)
  expect_identical(x$get_loaded(), TRUE)
  expect_identical(x$get_hidden(), FALSE)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_current(), 0.89)
  expect_identical(x$get_setting("factor"), x$get_factor())
  expect_identical(x$get_setting("status"), x$get_status())
  expect_identical(x$get_setting("current"), x$get_current())
  expect_identical(x$get_downloadable(), TRUE)
})

test_that("set methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = TRUE,
    status = FALSE,
    current = 0.89,
    factor = -0.2,
    id = "FID1"
  )
  # run tests
  x$set_factor(0.8)
  x$set_status(TRUE)
  x$set_visible(FALSE)
  x$set_invisible(100)
  x$set_loaded(TRUE)
  x$set_current(0.66)
  expect_identical(x$get_factor(), 0.8)
  expect_identical(x$get_status(), TRUE)
  expect_identical(x$get_visible(), FALSE)
  expect_identical(x$get_invisible(), 100)
  expect_identical(x$get_loaded(), TRUE)
  expect_identical(x$get_current(), 0.66)
  x$set_setting("factor", 0.3)
  x$set_setting("status", FALSE)
  x$set_setting("visible", TRUE)
  x$set_setting("current", 0.12)
  expect_identical(x$get_factor(), 0.3)
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_current(), 0.12)
})

test_that("export method", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = TRUE,
    hidden = FALSE,
    downloadable = TRUE,
    status = FALSE,
    current = 0.89,
    factor = -0.2,
    id = "FID1"
  )
  # run tests
  expect_equal(
    x$export(),
    list(
      name = "Human Footprint Index",
      variable = x$variable$export(),
      status = FALSE,
      visible = TRUE,
      hidden = FALSE,
      downloadable = TRUE,
      factor = -0.2
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "ha",
    legend = simulate_continuous_legend()
  )
  x <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    status = FALSE,
    factor = -0.2,
    id = "FID1"
  )
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FID1",
      name = "Human Footprint Index",
      status = FALSE,
      factor = -0.2,
      min_factor = -100,
      max_factor = 100,
      step_factor = 1,
      provenance = v$provenance$get_widget_data()
    )
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FID1",
      name = "Human Footprint Index",
      visible = TRUE,
      hidden = FALSE,
      legend = v$legend$get_widget_data(),
      units = "ha",
      provenance = v$provenance$get_widget_data(),
      type = "weight"
    )
  )
})
