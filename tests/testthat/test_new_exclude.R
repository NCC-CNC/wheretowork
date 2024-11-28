context("new_exclude")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_exclude_legend()
  )
  x <- new_exclude(
    name = "Urban areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    hidden = TRUE,
    status = FALSE,
    id = "FID1",
    overlap = NA_character_,
    downloadable = TRUE
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$name, "Urban areas")
  expect_identical(x$mandatory, TRUE)
  expect_identical(x$variable, v)
  expect_identical(x$visible, FALSE)
  expect_identical(x$invisible, NA_real_)
  expect_identical(x$loaded, FALSE)
  expect_identical(x$hidden, TRUE)
  expect_identical(x$status, FALSE)
  expect_identical(x$id, "FID1")
  expect_identical(x$overlap, NA_character_)
  expect_identical(x$downloadable, TRUE)
})

test_that("get methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_exclude_legend()
  )
  x <- new_exclude(
    name = "Urban areas",
    variable = v,
    mandatory = TRUE,
    visible = TRUE,
    hidden = FALSE,
    status = FALSE,
    id = "FID1",
    overlap = NA_character_,
    downloadable = TRUE
  )
  # run tests
  expect_identical(x$get_status(), FALSE)
  expect_identical(x$get_visible(), TRUE)
  expect_identical(x$get_invisible(), NA_real_)
  expect_identical(x$get_loaded(), TRUE)
  expect_identical(x$get_hidden(), FALSE)
  expect_identical(x$get_overlap(), NA_character_)
  expect_identical(x$get_downloadable(), TRUE)
})

test_that("set methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_exclude_legend()
  )
  x <- new_exclude(
    name = "Urban areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    status = FALSE,
    id = "FID1",
    overlap = NA_character_
  )
  # run tests
  x$set_status(TRUE)
  x$set_visible(FALSE)
  x$set_invisible(100)
  x$set_loaded(TRUE)
  expect_identical(x$get_status(), TRUE)
  expect_identical(x$get_visible(), FALSE)
  expect_identical(x$get_invisible(), 100)
  expect_identical(x$get_loaded(), TRUE)
})

test_that("export method", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_exclude_legend()
  )
  x <- new_exclude(
    name = "Urban areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    hidden = TRUE,
    downloadable = TRUE,
    status = FALSE,
    id = "FID1",
    overlap = NA_character_
  )
  # run tests
  expect_identical(
    x$export(),
    list(
      name = "Urban areas",
      variable = x$variable$export(),
      mandatory = TRUE,
      status = FALSE,
      visible = FALSE,
      hidden = TRUE,
      downloadable = TRUE,
      overlap = NA_character_
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable(
    dataset = d, index = 1, total = 200, units = "",
    legend = simulate_exclude_legend()
  )
  x <- new_exclude(
    name = "Urban areas",
    variable = v,
    mandatory = TRUE,
    visible = FALSE,
    hidden = FALSE,
    status = FALSE,
    id = "FID1",
    overlap = NA_character_
  )
  # run tests
  ## solution settings
  expect_identical(
    x$get_solution_settings_widget_data(),
    list(
      id = "FID1",
      name = "Urban areas",
      status = FALSE,
      mandatory = TRUE,
      provenance = v$provenance$get_widget_data(),
      overlap = NA_character_
    )
  )
  ## map manager settings
  expect_identical(
    x$get_map_manager_widget_data(),
    list(
      id = "FID1",
      name = "Urban areas",
      visible = FALSE,
      hidden = FALSE,
      legend = v$legend$get_widget_data(),
      units = "",
      provenance = v$provenance$get_widget_data(),
      type = "exclude"
    )
  )
})
