context("new_weight_results")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = FALSE,
    status = FALSE,
    current = 0.45,
    factor = -0.2,
    id = "FID1"
  )
  x <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "RID1")
  expect_identical(x$status, FALSE)
  expect_identical(x$factor, -0.2)
  expect_identical(x$held, 0.9)
  expect_identical(x$current, 0.45)
})

test_that("results methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = FALSE,
    status = FALSE,
    current = 0.45,
    factor = -0.2,
    id = "FID1"
  )
  w$factor <- 0.8
  w$status <- TRUE
  x <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  # run tests
  expect_identical(
    x$get_results_data(),
    tibble::tibble(
      name = "Human Footprint Index",
      status = TRUE,
      total = v$total,
      current = 0.45,
      factor = 0.8,
      held = 0.9,
      units = v$units,
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  w <- new_weight(
    name = "Human Footprint Index",
    variable = v,
    visible = FALSE,
    status = FALSE,
    current = 0.45,
    factor = -0.2,
    id = "FID1"
  )
  w$factor <- -0.8
  w$status <- TRUE
  x <- new_weight_results(
    weight = w,
    held = 0.9,
    id = "RID1"
  )
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID1",
      name = "Human Footprint Index",
      status = TRUE,
      factor = -0.8,
      total_amount = v$total,
      current_held = 0.45,
      solution_held = 0.9,
      units = v$units,
      provenance = v$provenance$get_widget_data(),
      type = "weight_results"
    )
  )
})
