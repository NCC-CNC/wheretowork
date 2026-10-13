context("calculate_variable_area_km2")

test_that("calculate_variable_area_km2", {
  # create object
  d <- new_dataset_from_auto(import_simple_raster_data())
  v <- new_variable_from_auto(dataset = d, index = 1)
  # calculate expected result
  values <- d$get_attribute_data()[[1]]
  areas <- d$get_planning_unit_areas()
  # run tests
  expect_equal(
    calculate_variable_area_km2(v),
    sum(areas[values > 0]) * 1e-6
  )
})

test_that("calculate_area_coverage", {
  # create data
  x <- c(1, 0, 1, 0)
  areas <- c(10, 20, 30, 40)
  data <- Matrix::sparseMatrix(
    i = c(1, 1, 1, 2, 2),
    j = c(1, 2, 3, 2, 4),
    x = c(1, 1, 0.5, 1, 1),
    dims = c(2, 4),
    dimnames = list(c("a", "b"), NULL)
  )
  # run tests
  ## proportion of area selected
  expect_equal(
    calculate_area_coverage(x, data, areas),
    c(a = (10 + 30) / (10 + 20 + 30), b = 0)
  )
  ## proportion multiplied by total area gives actual area selected
  expect_equal(
    calculate_area_coverage(x, data, areas)[["a"]] * (10 + 20 + 30),
    10 + 30
  )
  ## no data
  expect_identical(
    calculate_area_coverage(x, data[0, , drop = FALSE], areas),
    numeric(0)
  )
})
