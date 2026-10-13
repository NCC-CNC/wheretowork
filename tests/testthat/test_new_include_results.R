context("new_include_results")

test_that("initialization", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  i <- new_include(
    name = "Protected Areas",
    variable = v,
    visible = FALSE,
    status = FALSE,
    id = "I1"
  )
  x <- new_include_results(
    include = i,
    held = 0.9,
    id = "RID1"
  )
  # run tests
  print(x)
  expect_is(x$repr(), "character")
  expect_identical(x$id, "RID1")
  expect_identical(x$status, FALSE)
  expect_identical(x$held, 0.9)
})

test_that("results methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  i <- new_include(
    name = "Protected Areas",
    variable = v,
    visible = FALSE,
    status = FALSE,
    id = "I1"
  )
  x <- new_include_results(
    include = i,
    held = 0.9,
    id = "RID1"
  )
  # calculate expected area
  expected_area <- sum(
    d$get_planning_unit_areas()[which(d$get_attribute_data()[[1]] > 0)]
  ) * 1e-6
  # run tests
  expect_identical(
    x$get_results_data(),
    tibble::tibble(
      name = "Protected Areas",
      status = FALSE,
      total = expected_area,
      held = 0.9,
      units = stringi::stri_unescape_unicode("km\\u00B2"),
    )
  )
})

test_that("widget methods", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1, units = "ha")
  i <- new_include(
    name = "Protected Areas",
    variable = v,
    visible = FALSE,
    status = TRUE,
    id = "I1"
  )
  x <- new_include_results(
    include = i,
    held = 0.9,
    id = "RID1"
  )
  # calculate expected area
  expected_area <- sum(
    d$get_planning_unit_areas()[which(d$get_attribute_data()[[1]] > 0)]
  ) * 1e-6
  # run tests
  expect_identical(
    x$get_widget_data(),
    list(
      id = "RID1",
      name = "Protected Areas",
      status = TRUE,
      total_amount = expected_area,
      solution_held = 0.9,
      units = stringi::stri_unescape_unicode("km\\u00B2"),
      provenance = v$provenance$get_widget_data(),
      type = "include_results"
    )
  )
})
